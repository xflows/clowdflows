from django.shortcuts import render
import json

def cforange_filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/cforange_filter_integers.html',{'widget':widget,'intList':input_dict['intList']})

def cforange_hierarchical_clustering(request,input_dict,output_dict,widget):
    import orange
    matrix = input_dict['dm']
    root = orange.HierarchicalClustering(matrix, linkage=orange.HierarchicalClustering.Average)
    def build_hierarchy(node, root=False):
        return {
            'name' : 'root' if root else '',
            'height' : node.height if node.branches else 0,
            'children' : [build_hierarchy(node.left), build_hierarchy(node.right)] if node.branches else []
        }
    hierarchy = json.dumps(build_hierarchy(root, root=True))
    return render(request, 'interactions/cforange_hierarchical_clustering.html', {'widget' : widget, 'hierarchy' : hierarchy})