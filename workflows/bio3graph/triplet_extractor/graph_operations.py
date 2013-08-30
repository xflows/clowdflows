import os
import StringIO
import networkx as nx


OPTLINE = '#'
CANVAS = '_canvas'
ATTRS = '_attributes'
DB = '_database'

ARCREL = 'relation'
LINECOLOR = 'linecolor'

BLACK = '0/0/0'
RED = '255/0/0'
GREEN = '0/255/0'
BLUE = '0/0/255'
PINK = '255/0/255'

def load_BMG_to_networkx(data):
    fp = StringIO.StringIO()
    if os.path.exists(data):
        fp.write(open(data).read())
    else:
        fp.write(data)
    fp.flush()
    fp.seek(0)

    #assert(os.path.exists(fname))
    #fp = open(fname)
    lines = fp.readlines()
    fp.close()
    #print lines

    graph = nx.MultiDiGraph()
    for line in lines:
        line = line.strip()
        if not line:
            continue
        elts = line.split()

        if elts[0] == OPTLINE:
            if elts[1] == CANVAS:
                continue
            elif elts[1] == ATTRS:
                node = elts[2]
                attrs = elts[3:]
                if not graph.has_node(node):
                    continue

                for atr in attrs:
                    parts = atr.split('=')
                    graph.node[node][parts[0]] = parts[1]
            else:
                # unknown tags
                continue
        #edge data
        else:
            # skip lines with node announcements (source nodes)
            if len(elts) < 3:
                continue
            else:
                n1 = elts[0]
                n2 = elts[1]
                rel = elts[2]
                attrs = elts[3:]

                if not graph.has_node(n1):
                    graph.add_node(n1)
                if not graph.has_node(n2):
                    graph.add_node(n2)
                #graph.add_edge(n1, n2)
                #graph[n1][n2][ARCREL] = rel

                atrDict = {}
                for atr in attrs:
                    parts = atr.split('=')
                    atrDict[parts[0]] = parts[1]
                    #graph[n1][n2][parts[0]] = parts[1]

                #print 'adding arc: ', n1, n2, rel, atrDict
                graph.add_edge(n1, n2, key=rel, attr_dict=atrDict)

    return graph
#end


def reset_edge_colors(graph, basecolor=BLACK):
    assert(isinstance(graph, nx.Graph))

    for fr in graph.edge:
        for to in graph.edge[fr]:
            for relType in graph.edge[fr][to]:
                graph.edge[fr][to][relType][LINECOLOR] = basecolor
#end


def colour_relations(graph, relations, colour=PINK):
    assert(isinstance(graph, nx.Graph))
    for (fr, to, rel) in relations:
        if graph.has_edge(fr, to, rel):
            graph.edge[fr][to][rel][LINECOLOR] = colour
    return graph
#end


def export_to_BMG(graph):
    assert(isinstance(graph, nx.Graph))

    data = StringIO.StringIO()

    # export arcs and their attributes
    for fr in graph.edge:
        for to in graph.edge[fr]:
            for relType in graph.edge[fr][to]:
                line = '%s %s %s' % (fr, to, relType)
                for (atr, val) in graph.edge[fr][to][relType].items():
                    line += ' %s=%s' % (atr, val)
                line += '\n'
                data.write(line)

    # export node attributes
    for node in graph.nodes_iter():
        line = '%s %s %s' % (OPTLINE, ATTRS, node)
        for (atr, val) in graph.node[node].items():
            line += ' %s=%s' % (atr, val)
        line += '\n'
        data.write(line)

    data.flush()
    return data.getvalue()
#end


def merge_incremental_graph(old, new, oldColor=BLACK, overlapColor=GREEN, newColor=RED):
    assert(isinstance(old, nx.Graph))
    assert(isinstance(new, nx.Graph))

    graph = nx.copy.deepcopy(old)
    reset_edge_colors(graph, basecolor=oldColor)

    # merge nodes
    for node in new.nodes_iter():
        # if the node is new, add it with all attributes
        # if not, do not modify existing attributes (?)
        if not graph.has_node(node):
            graph.add_node(node, attr_dict=new.node[node])
            #print 'novo: ', node

    for fr in new.edge:
        for to in new.edge[fr]:
            for relType in new.edge[fr][to]:
                # overlap
                if graph.has_edge(fr, to, relType):
                    graph.edge[fr][to][relType][LINECOLOR] = overlapColor
                    #print 'overlap: ', fr, to
                # new
                else:
                    graph.add_edge(fr, to, key=relType, attr_dict=new[fr][to][relType])
                    graph.edge[fr][to][relType][LINECOLOR] = newColor
                    #print 'new: ', fr, to
    return graph
#end


def merge(a, b):
    assert(isinstance(a, nx.Graph))
    assert(isinstance(b, nx.Graph))

    result = nx.copy.deepcopy(a)

    # merge nodes
    for node in b.nodes_iter():
        # if the node is new, add it with all attributes
        if not result.has_node(node):
            result.add_node(node, attr_dict=b.node[node])
        # if not, update the attributes
        else:
            #result.node[node].update(b.node[node])
            pass

    # merge edges
    for fr in b.edge:
        for to in b.edge[fr]:
            for relType in b.edge[fr][to]:
                if not result.has_edge(fr, to, relType):
                    #print 'adding: ', fr, to, relType
                    result.add_edge(fr, to, key=relType, attr_dict=b.edge[fr][to][relType])
                else:
                    #print 'updating: ', fr, to, relType, b.edge[fr][to][relType]
                    #result.edge[fr][to][relType].update(b.edge[fr][to][relType])
                    pass
    return result
#end



# removes edges which are not of given type
def filter_edge_types(graph, etype):
    result = nx.copy.deepcopy(graph)
    assert(isinstance(result, nx.Graph))

    for fr in graph.edge:
        for to in graph.edge[fr]:
            for relType in graph.edge[fr][to]:
                if relType != etype:
                    result.remove_edge(fr, to, relType)
    return result
#end



# this function removes transitive relations introduced by 'new' into 'graph'
#
def find_transitive_relations(graph, new, relTypes=['A', 'I']):
    trels = []
    for relType in relTypes:
        #print 'relation: ', relType
        relGraph = filter_edge_types(graph, relType)
        relNew = filter_edge_types(new, relType)
        for (fr,to) in relNew.edges_iter():
            # a duplicate edge, skip it and save time
            if relGraph.has_edge(fr, to):
                continue

            #print 'considering ', fr, to
            if nx.has_path(relGraph, fr, to):
                trels.append((fr, to, relType))

            #try:
                #p = nx.shortest_path(relGraph, fr, to)
            #except nx.NetworkXNoPath:
                #continue
            #else:
                #if len(p) > 2: # this is for the same edges
                    #trels.append((fr, to, relType))
    return trels

#end

#a = load_BMG_to_networkx('g1.bmg')
#b = load_BMG_to_networkx('g2.bmg')
#print find_transitive_relations(a, b)




#a = load_BMG_to_networkx('SA_ET_crosstalk_JA_mali_space.bmg')
#reset_edge_colors(a)
#s = export_to_BMG(a)
#fp = open('test.bmg', 'w')
#fp.write(s)
#fp.close()


#a = load_BMG_to_networkx('graph1.bmg')
#b = load_BMG_to_networkx('graph2.bmg')
#e = merge(a,b)
#fp = open('graph123.bmg', 'w')
#fp.write(export_to_BMG(e))
#fp.close()


#g = merge_incremental_graph(a, b)
#s = export_to_BMG(g)
#fp = open('graph12.bmg', 'w')
#fp.write(s)
#fp.close()



#rels = [('component_eds1', 'component_sa', 'A'),
#('component_eds1', 'component_pr', 'A'),
#('component_npr1', 'component_pr', 'A'),
#('component_eds5', 'component_pr1', 'A'),
#('component_ja', 'component_pdf1.2', 'A'),
#('component_ja', 'component_vsp', 'A'),
#('component_ja', 'component_thi2.1', 'A'),
#('component_sa', 'component_tga2', 'A'),
#('component_sa', 'component_pr', 'A'),
#('component_sa', 'component_pr1', 'A'),
#('component_ethylene', 'component_hls1', 'A'),
#('component_ethylene', 'component_pdf1.2', 'A'),
#('component_ethylene', 'component_arf2', 'A'),
#('component_ethylene', 'component_ein2', 'A'),
#('component_ethylene', 'component_ein3', 'A'),
#('component_ethylene', 'component_erf1', 'A'),
#('component_sid2', 'component_pr1', 'A'),
#('component_sid2', 'component_pr', 'A'),
#('component_ein2', 'component_erf1', 'A'),
#('component_ctr1', 'component_erf1', 'A'),
#('component_mapk', 'component_ein2', 'A'),
#('component_mapk', 'component_ein3', 'A'),
#('component_mapk', 'component_erf1', 'A'),
#('component_pad4', 'component_npr1', 'A'),
#('component_pad4', 'component_sa', 'A')]


#a = load_BMG_to_networkx('before_nov+triplets.bmg')
#g = colour_relations(a, rels)