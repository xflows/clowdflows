import StringIO
import networkx as nx

# from networkx.readwrite import json_graph
import json

OPTLINE = '#'
CANVAS = '_canvas'
ATTRS = '_attributes'
DB = '_database'

ARCREL = 'relation'
LINECOLOR = 'linecolor'
FILL = 'fill'

BLACK = '0/0/0'
RED = '255/0/0'
GREEN = '0/255/0'
BLUE = '0/0/255'
PINK = '255/0/255'
LIGHTBLUE = '0/125/255'
YELLOW = '255/230/45'

BM_NODE_DATA = {
    'Node': {'name': 'Node', 'color': '#f0f0f0'},
    'Sequence': {'name': 'Sequence', 'color': '#d3f7a3'},
    'Gene': {'name': 'Gene', 'color': '#d3f7a3'},
    'Protein': {'name': 'Protein', 'color': '#c7f0b2'},
    'Enzyme': {'name': 'Enzyme', 'color': '#c1eaae'},
    'ProteinGroup': {'name': 'Protein group', 'color': '#c7f0b2'},
    'AllelicVariant': {'name': 'Allelic variant', 'color': '#c7f0b2'},
    'Article': {'name': 'Article', 'color': '#fae6a0'},
    'HomologGroup': {'name': 'Homolog group', 'color': '#bbedd7'},
    'OrthologGroup': {'name': 'Ortholog group', 'color': '#c2edda'},
    'GO': {'name': 'GO', 'color': '#b9daea'},
    'BiologicalProcess': {'name': 'Biological process', 'color': '#b9daea'},
    'MolecularFunction': {'name': 'Molecular function', 'color': '#bddfef'},
    'CellularComponent': {'name': 'Cellular component', 'color': '#aed6ea'},
    'Ligand': {'name': 'Ligand', 'color': '#d2cbf0'},
    'Substance': {'name': 'Substance', 'color': '#d2cbf0'},
    'Compound': {'name': 'Compound', 'color': '#d2cbf0'},
    'Drug': {'name': 'Drug', 'color': '#cbc4e9'},
    'Glycan': {'name': 'Glycan', 'color': '#c6c8f2'},
    'GenomicContext': {'name': 'Genomic context', 'color': '#ffd7fd'},
    'Locus': {'name': 'Locus', 'color': '#b4c4ef'},
    'Phenotype': {'name': 'Phenotype', 'color': '#c1d1ff'},
    'Locus/Phenotype': {'name': 'Locus/Phenotype', 'color': '#c3c7f2'},
    'Gene/Phenotype': {'name': 'Gene/Phenotype', 'color': '#d3f7a3'},
    'Family': {'name': 'Family', 'color': '#b3e2c0'},
    'Region': {'name': 'Region', 'color': '#c3e5cc'},
    'Domain': {'name': 'Domain', 'color': '#c3e5cc'},
    'Repeat': {'name': 'Repeat', 'color': '#c3e5cc'},
    'Group': {'name': 'Group', 'color': '#f0f0f0'},  # this one is strange, was added manually
    'Site': {'name': 'Site', 'color': '#bee5c9'},
    'ActiveSite': {'name': 'Active site', 'color': '#bee5c9'},
    'BindingSite': {'name': 'Binding site', 'color': '#bee5c9'},
    'PostTranslationalModification': {'name': 'Post-translational modification', 'color': '#bee5c9'},
    'Pathway': {'name': 'Pathway', 'color': '#d0b9e7'},
    'Tissue': {'name': 'Tissue', 'color': '#e5dabd'},
    'Organism': {'name': 'Organism', 'color': '#e5d7b1'},
    'MeSHHeading': {'name': 'MeSH heading', 'color': '#efe7b0'},
    'OMIM': {'name': 'OMIM', 'color': '#efe7b0'}
}

SYMMETRIC_EDGE_TYPES = ['interacts_with', 'is_related_to', 'is_homologous_to', 'overlaps', 'has_synonym', 'functionally_associated_to']


def load_BMG_to_networkx(data):
    fp = StringIO.StringIO()
    fp.write(data)
    fp.flush()
    fp.seek(0)

    lines = fp.readlines()
    fp.close()

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
                    graph.node[node][parts[0]] = parts[1].replace('+', ' ')
            else:
                # unknown tags
                continue
        # edge data
        else:
            # skip lines with node announcements (source nodes)
            if len(elts) < 3:
                continue
            else:
                n1 = elts[0]
                n2 = elts[1]
                rel = elts[2]
                attrs = elts[3:]

                symmetric = True if rel in SYMMETRIC_EDGE_TYPES else False

                if not graph.has_node(n1):
                    graph.add_node(n1)
                if not graph.has_node(n2):
                    graph.add_node(n2)

                atrDict = {}
                for atr in attrs:
                    parts = atr.split('=')
                    atrDict[parts[0]] = parts[1].replace('+', ' ')
                    # graph[n1][n2][parts[0]] = parts[1]

                # NetworkX does not support mixed graphs
                atrDict['symmetric'] = symmetric

                # handle also the reverse case
                if rel.startswith('-'):
                    rel = rel[1:]
                    graph.add_edge(n2, n1, key=rel, attr_dict=atrDict)
                else:
                    graph.add_edge(n1, n2, key=rel, attr_dict=atrDict)
                    # simulate mixed graphs
                    if symmetric:
                        graph.add_edge(n2, n1, key=rel, attr_dict=atrDict)

    return graph
# end


def export_to_BMG(graph):
    assert (isinstance(graph, nx.Graph))

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
            line += ' %s=%s' % (atr, val.replace(' ', '+'))
        line += '\n'
        data.write(line)

    data.flush()
    return data.getvalue()
# end


# graph 'g' must be a Biomine graph!
def group_nodes(g):
    # useful for finding equivalent nodes (same neighbours, same edges)
    def inout_edges(g, node):
        inedges = []
        for (fr, to, typ) in g.in_edges_iter(node, keys=True):
            inedges.append((fr, typ))
        outedges = []
        for (fr, to, typ) in g.out_edges_iter(node, keys=True):
            outedges.append((to, typ))
        return sorted(inedges), sorted(outedges)

    # end

    assert(isinstance(g, nx.Graph))

    # cluster node types
    ntypes = {}
    for node in g.nodes_iter():
        #if g.in_degree(node) > 0:
            #continue
        #prefix = node[:node.index('_')]
        prefix = node[:node.index(':')]
        if prefix not in ntypes:
            ntypes[prefix] = [node]
        else:
            ntypes[prefix].append(node)

    # not optimal, should be grouped also according to the number of in/out edges and the according to their type...
    groups = []
    for nt in ntypes:
        nodes = ntypes[nt]
        while nodes != []:
            current = nodes.pop()
            group = [current]
            ioc = inout_edges(g, current)
            for cand in nodes:
                # check if in-out edges are the same
                if ioc == inout_edges(g, cand):
                    group.append(cand)
            nodes = list(set(nodes) - set(group))
            if len(group) > 1:
                groups.append(group)

    for group in groups:
        rep = group[0]
        prefix = rep[:rep.index('_')]
        db = rep[rep.index('_')+1:rep.index(':')]
        size = len(group)
        new = '_'.join(['Group', prefix, '(%s)' % db, 'x%d' % size])
        label = '||'.join([name for name in group])
        g.add_node(new)
        g.node[new]['PrimaryName'] = label
        for (fr, to, typ, data) in g.out_edges_iter(rep, keys=True, data=True):
            g.add_edge(new, to, key=typ, attr_dict=data)
        for (fr, to, typ, data) in g.in_edges_iter(rep, keys=True, data=True):
            g.add_edge(fr, new, key=typ, attr_dict=data)
        g.remove_nodes_from(group)
# end


def prepare_for_visjs(g):
    assert (isinstance(g, nx.Graph))

    # add ids so we can refer to nodes easily when building vis.js data structure
    id2bmname = {}
    for i, node in enumerate(g.nodes_iter()):
        g.node[node]['id'] = i
        id2bmname[i] = node

    # find groups
    nodegroups = {}
    for node in g.nodes_iter():
        for prefix in BM_NODE_DATA:
            if node.lower().startswith(prefix.lower()):
                nodegroups[node] = BM_NODE_DATA[prefix]['name']
        if nodegroups.get(node) is None:
            nodegroups[node] = 'Node'

    # construct node data
    nodes = []
    for node in g.nodes_iter():
        sname = g.node[node].get('ShortName', '')
        sname = '\n' + sname if sname else sname

        # write the type above
        # name = nodegroups[node]
        i = node.find('_')
        if i != -1:
            name = name = nodegroups[node] + '\n' + node[i + 1:]
        else:
            name = node
        new = {'id': g.node[node]['id'],
               'label': name + sname,
               'bmname': node,
               'type': nodegroups[node],
               'PrimaryName': g.node[node].get('PrimaryName', ''),
               'ShortName': g.node[node].get('ShortName', ''),
               'goodness': g.node[node].get('goodness', ''),
               'group': nodegroups[node],
               'shape': 'box'
        }
        nodes.append(new)

    # construct edge data
    arcs = []
    for fromnode, tonode, etype in g.edges_iter(keys=True):
        # do not add both arcs of an undirected edge of the mixed graph simulation
        if g.edge[fromnode][tonode][etype]['symmetric']:
            # print fromnode, tonode, etype
            skip = False
            for info in arcs:
                if info['from'] == g.node[tonode]['id'] and info['to'] == g.node[fromnode]['id']:
                    skip = True
                    break
            if skip:
                continue

        estyle = 'line' if etype in SYMMETRIC_EDGE_TYPES else 'arrow'
        new = {'from': g.node[fromnode]['id'],
               'to': g.node[tonode]['id'],
               'label': etype.replace('_', ' '),
               'goodness': g.edge[fromnode][tonode][etype].get('goodness', ''),
               'rarity': g.edge[fromnode][tonode][etype].get('rarity', ''),
               'reliability': g.edge[fromnode][tonode][etype].get('reliability', ''),
               'relevance': g.edge[fromnode][tonode][etype].get('relevance', ''),
               'source_db_name': g.edge[fromnode][tonode][etype].get('source_db_name', ''),
               'source_db_version': g.edge[fromnode][tonode][etype].get('source_db_version', ''),
               'style': estyle,
               'color': {'color': 'black', 'highlight': 'blue', 'hover': 'blue'}
        }
        arcs.append(new)

    groups = {}
    for prefix in BM_NODE_DATA:
        groups[prefix] = {'color': {'border': BM_NODE_DATA[prefix]['color'],
                                    'background': BM_NODE_DATA[prefix]['color'],
                                    'hover': {'background': '#FF9B8C', 'border': 'red'},
                                    'highlight': '#FF6464'},
                          'shape': 'box'}

    return {'nodes': nodes,
            'arcs': arcs,
            'id2bmname': id2bmname,
            'groups': groups}
# end
