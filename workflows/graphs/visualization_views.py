from django.shortcuts import render

def graphs_display_summation(request,input_dict,output_dict,widget):
    if sum(input_dict['intList']) == input_dict['sum']:
        check = 'The calculation appears correct.'
    else:
        check = 'The calculation appears incorrect!'
    return render(request, 'visualizations/graphs_display_integers.html',{'widget':widget,'input_dict':input_dict, 'output_dict':output_dict, 'check':check})



################################################


def graphs_visualize_visjs(request, input_dict, output_dict, widget):
    import networkx as nx

    def prepare_visjs_data(g):
        nodes = []
        for node in g.nodes_iter():
            new = {'id': str(node),
                   'label': str(g.node[node]['label']) if 'label' in g.node[node] else str(node),
                   'shape': 'box'
            }
            nodes.append(new)

        edges = []
        for fromnode, tonode, etype in g.edges_iter(keys=True):
            if 'label' in g[fromnode][tonode][etype]:
                label = str(g[fromnode][tonode][etype]['label'])
            elif 'weight' in g[fromnode][tonode][etype]:
                label = str(g[fromnode][tonode][etype]['weight'])
            else:
                #label = str(etype)
                label = ''
            new = {'from': str(fromnode),
                   'to': str(tonode),
                   'label': label,
                   'color': {'color': 'black', 'highlight': 'blue', 'hover': 'blue'},
            }
            if nx.is_directed(g):
                new['arrows'] = 'to'
            edges.append(new)

        return {'nodes': nodes,
                'edges': edges}
    # end


    g = input_dict['graph']
    gtext = input_dict['gtext']
    if not g:
        g = nx.parse_pajek(gtext)
    data = prepare_visjs_data(g)

    return render(request, 'visualizations/graphs_visualize_visjs.html',
                  {'widget': widget,
                   'nodes': data['nodes'],
                   'edges': data['edges']
                   })
# end



