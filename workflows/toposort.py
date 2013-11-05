# -*- coding: utf-8 -*-
from __future__ import absolute_import
from __future__ import with_statement

__all__ = ['toposort', 'CycleDetectedError', 'LoopDetectedError']

###################################################################

class CycleDetectedError(Exception): pass
class LoopDetectedError(Exception): pass

def toposort(edges, extra_nodes=None,
             non_loop_cycles_are_errors=False,
             loops_are_errors=False,
             loop_report=None,
             cycle_report=None):
    """Computes a topological sorting of a directed graph.

    The topological sorting is an ordering of the graph's nodes such that,
    for every edge (u -> v), u comes before v.
    The implementation is hardened against loops and arbitrary cycles and
    can handle isolated nodes and complete (sub)graphs. In these
    degenerated cases the computed ordering is obviously not correct
    (as there is no correct one), but it will contain all nodes and the nodes
    not participating in cycles will be ordered correctly.
    The detected cycles can be reported, but it is not guaranteed that every
    cycle is reported (this does not mean a cycle can ever go undetected).
    Note that the node objects must be hashable and properly compare by
    identity with the ``is`` operator.
    
    The basic algorithm is a depth-first search along the reversed
    edges, starting from nodes that only have incoming edges.
    With this kind of traversal, the result ordering is added to whenever
    a recursion frame ends.
    Additionally we employ cycle detection by tracking which nodes have
    already been visited *within the same recursion tree path*.
    This mechanism can both detect cycles and determine the participating
    nodes. To do this robustly and not run into infinite recursion we globally
    track which edges have been traversed so far and skip those later.

    Args:
        * edges: the directed edges of the graph (an iterable of iterables).
          The first two elements of each edges element are interpreted as
          directed edge (1st elem -> 2nd elem).
        * extra_nodes: if specified, an iterable with any additional (or
          duplicated) nodes. This is entirely optional and this sequence does
          not have to include all or any nodes specified in the edges,
          but if used, this function can include in the sorting those nodes
          without incoming or outgoing edges (i.e., isolated nodes,
          with nodes of degree 2 with a loop also considered isolated).
        * non_loop_cycles_are_errors: if specified and True, non-loop cycles are
          detected and a CycleDetectedError is raised. Normally, edges
          completing a cycle in a traveral are ignored,
          Since there exists no topological ordering for a graph with cyles
          the returning sequence is not correct, but it will contain all nodes
          and it will be correct for the nodes that do not make up cycles.
          Note that this argument has no effect on the handling of loops which
          are ignored by default.
        * loops_are_errors: if specified and True, loops are detected
          and a LoopDetectedError is raised. Normally, loops are ignored.
        * loop_report: if specified this argument should be a list that any
          detected nodes with loops are appended to.
        * cycle_report: if specified this argument should be a list that any
          detected cycles are appended to (as a list of nodes for each cycle).
          Note that there is no guarantee that all cycles in the graph will be
          reported.

    Returns:
        A list of all nodes in topological order or, if no such order exists,
        one that comes reasonably close.

    Raises:
        * LoopDetectedError: if loops_are_errors is True and a loop is detected.
          If the loop_report argument was specified and this exception is
          raised, the report will contain the loop.
        * CycleDetectedError: if non_loop_cycles_are_errors is True and a
          cycle is detected. If the cycle_report argument was specified and
          this exception is raised, the report will contain the cycle.
    """
    
    # Init with extra_nodes to be sure to have all isolated nodes.
    all_nodes = set([] if extra_nodes is None else extra_nodes)

    # Make the edge list unique
    temp = set()
    for edge in edges:
        temp.add(edge)
    edges = list(temp)
    temp = 0
    
    # Holds the incoming edges for each node.
    # node -> [(edge index, source node)] 
    edges_towards = {}
    
    # All nodes with at least one outgoing edge.
    source_nodes = set()
   
    if loop_report is not None:
        loop_report_set = set()
  
    # Fill the preceeding data structures, assigning a numeric
    # index to each edge for easier tracking.
    for edge_index, edge in enumerate(edges):
        
        # Extract endpoints
        edge_iter = iter(edge)
        a = edge_iter.next()
        b = edge_iter.next()
        all_nodes.add(a)
        
        if a is b:
            if loop_report is not None:
                loop_report_set.add(a)
            if loops_are_errors:
                if loop_report is not None:
                    loop_report += list(loop_report_set)
                raise LoopDetectedError('loop at node "%s"' % a)
            continue
        
        all_nodes.add(b)
        source_nodes.add(a)
        
        if b in edges_towards:
            edges_towards[b].append((edge_index, a))
        else:
            edges_towards[b] = [(edge_index, a)]
            
    if loop_report is not None:
        loop_report += list(loop_report_set)
        loop_report_set = None # Don't need this anymore.

    sink_nodes = all_nodes - source_nodes
    result_sorting = []
    
    traversed_edges = set() # edge indices
    visited_nodes = set()

    class CycleDetector(object):
        # The standard cycle detection technique:
        # track the nodes visited *within the same call subtree*
        # and if we come across a node twice, we've found a circle.
       
        def __init__(self):
            self._seen_set = set()
            self._seen_list = list()
            
        def push(self, node):
            if node in self._seen_set:
                # A cycle!
                if cycle_report is not None:
                    cycle = self._seen_list[self._seen_list.index(node):]
                    cycle.reverse()
                    cycle_report.append(cycle)
                if non_loop_cycles_are_errors:
                    raise CycleDetectedError()
                return True
            else:
                self._seen_set.add(node)
                self._seen_list.append(node)
                return False
        
        def pop(self, node, caused_cycle):
            # This is a bit tricky: we must be sure to only pop
            # the stack and set if we actually added to it in push().
            # To track this information we simply use the caller's
            # stack frame by making him pass us the flag back.
            if not caused_cycle:
                del self._seen_list[-1:]
                self._seen_set.difference_update([node])


    # The dummy list trick so we can assign to this
    # in a nested function.
    cycle_detector = [None]

    def visit(node):
        caused_cycle = cycle_detector[0].push(node)
        
        if not caused_cycle:
            # Recurse
            towards = edges_towards.get(node, None)
            if towards is not None:
                for edge_index, source_node in towards:
                    if edge_index not in traversed_edges:
                        traversed_edges.add(edge_index)
                        visit(source_node)
        # Add to result on our way back up recursion creek:
        if node not in visited_nodes:
            visited_nodes.add(node)
            result_sorting.append(node)
        cycle_detector[0].pop(node, caused_cycle)

    def start_traversal(root_node):
        cycle_detector[0] = CycleDetector()
        visit(root_node)

    # Traverse from sinks.
    # In cycle-free graphs this should be enough already.
    for node in sink_nodes:
        start_traversal(node)

    # Have all nodes been visited?
    # We must check this because there might be components without
    # sink nodes or no sink nodes in the entire graph.
    if len(visited_nodes) < len(all_nodes):
        for n in all_nodes:
            if n not in visited_nodes:
                start_traversal(n)
    return result_sorting

###################################################################

def test_generic_stress():
    
    from nose.tools import assert_equal
    import random

    def generic_test(num_nodes, num_edges):
        """Creates a random cycle-free graph with the specified starting number
        of by dropping a random edge from a detected cycle until no more cycles
        are detected, then check ordering conditions."""
        edges = []
        for _ in xrange(num_edges):
            n1 = random.randint(0, num_nodes - 1)
            n2 = random.randint(0, num_nodes - 1)
            edges.append((n1, n2))
        while True:
            try:
                loops = []
                cycles = []
                ordering = toposort(edges, range(num_nodes),
                                    non_loop_cycles_are_errors=True,
                                    loop_report=loops,
                                    cycle_report=cycles)
            except CycleDetectedError:
                edges.remove(tuple(cycles[0][0:2]))
            else:
                break
        
        # All nodes contained?
        assert_equal(set(ordering), set(range(num_nodes)))

        # All edge conditions satisfied?
        for edge in edges:
            n1, n2 = edge
            if n1 == n2:
                assert n1 in loops
            else:
                assert ordering.index(n1) < ordering.index(n2)
    
    random.seed(4711)
    for _ in xrange(100):
        generic_test(random.randint(1, 200), random.randint(0, 500))