from django.shortcuts import render
import json

def cforange_filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/cforange_filter_integers.html',{'widget':widget,'intList':input_dict['intList']})

def cforange_hierarchical_clustering(request,input_dict,output_dict,widget):
    import orange
    matrix = input_dict['dm']
    linkage = int(input_dict['linkage'])
    linkages = [("Single linkage", orange.HierarchicalClustering.Single),
                       ("Average linkage", orange.HierarchicalClustering.Average),
                       ("Ward's linkage", orange.HierarchicalClustering.Ward),
                       ("Complete linkage", orange.HierarchicalClustering.Complete),
                      ]    
    root = orange.HierarchicalClustering(matrix, linkage=linkages[linkage][1])
    attributes = [x.name for x in matrix.items.domain]
    def build_hierarchy(node, root=False):
        values_dict = dict([(x,matrix.items[node.first][x].value) for x in attributes]) if not node.branches else {}
        for attribute in values_dict.keys():
            if type(values_dict[attribute]) == float:
                values_dict[attribute]="%.3f" % values_dict[attribute]
        return {
            'name' : 'root' if root else '',
            'id' : node.first if not node.branches else -1,
            'height' : node.height if node.branches else 0,
            'children' : [build_hierarchy(node.left), build_hierarchy(node.right)] if node.branches else [],
            'values' : values_dict,
            'leaf' : True if not node.branches else False
        }
    hierarchy = json.dumps(build_hierarchy(root, root=True))
    return render(request, 'interactions/cforange_hierarchical_clustering.html', {'widget' : widget, 'hierarchy' : hierarchy, 'attributes':attributes,'vizualization':input_dict['visualization']})