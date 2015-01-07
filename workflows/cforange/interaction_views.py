from django.shortcuts import render
import json

def cforange_filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/cforange_filter_integers.html',{'widget':widget,'intList':input_dict['intList']})

def cforange_hierarchical_clustering(request,input_dict,output_dict,widget):
    import Orange, orange, sys
    from library import Clustering

    sys.setrecursionlimit(10000)

    ##try:
    #iris = Orange.data.Table("vehicle")
    ##iris = Orange.data.Table("iris") 
    #print len(iris)
    #m = Orange.misc.SymMatrix(len(iris))
    #m = Orange.distance.distance_matrix(iris, Orange.distance.Euclidean)
    #matrix = m
    ######################
    matrix = input_dict['dm']
    #print matrix
    linkage = int(input_dict['linkage'])
    root = Clustering.hierarchical_clustering(linkage, matrix)
    dm_examples = True
    ######################

    
    try:
        #attributes = [x.name for x in matrix.items.domain]
        attributes = [x.name for x in matrix.items.domain] + [m.name for m in matrix.items.domain.getmetas().values()]
    except:
        attributes = ['attribute']
        dm_examples = False
    
    """
    def build_hierarchy(node, root=False):
        if dm_examples:
            values_dict = dict([(x,matrix.items[node.first][x].value) for x in attributes]) if node.branches != None and (not node.branches) else {}
        else:
            try:
                values_dict = dict([(x,matrix.items[node.first].name) for x in attributes]) if node.branches != None and (not node.branches) else {}
            except AttributeError as e:
                print e
                print sys.exc_info()[0]
        for attribute in values_dict.keys():
            if type(values_dict[attribute]) == float:
                values_dict[attribute]="%.3f" % values_dict[attribute]

        try:
            ret = {
                'name' : 'root' if root else '',
                'id' : node.first if not node.branches else -1,
                'height' : node.height if node.branches else 0,
                'children' : [build_hierarchy(node.left), build_hierarchy(node.right)] if node.branches else [],
                'values' : values_dict,
                'leaf' : True if (not node.branches is None) and not node.branches else False
            }
            print "returning"
        except:
            print sys.exc_info()[0]
            ret = {} 

        return ret
    
    hierarchy = json.dumps(build_hierarchy(root, root=True))
    
    """
    def build_hierarchy2(node, position, root=False):
        if dm_examples:
            values_dict = dict([(x,matrix.items[node.first][x].value) for x in attributes]) if node.branches != None and (not node.branches) else {}
        else:
            try:
                values_dict = dict([(x,matrix.items[node.first].name) for x in attributes]) if node.branches != None and (not node.branches) else {}
            except AttributeError as e:
                print e, sys.exc_info()[0]

        for attribute in values_dict.keys():
            if type(values_dict[attribute]) == float:
                values_dict[attribute]="%.3f" % values_dict[attribute]

        ret = {
            'name' : 'root' if root else '',
            'id' : node.first if not node.branches else -1,
            'height' : node.height if node.branches else 0,
            #'children' : [build_hierarchy(node.left), build_hierarchy(node.right)] if node.branches else [],
            'parent' : position,
            'children' : [position+1, position+2] if node.branches else [],
            'values' : values_dict,
            'leaf' : True if (node.branches is None) else False
        }
        #print ret
        return ret

    hierarchy = [build_hierarchy2(root, 0, root=True)]  #json.dumps(build_hierarchy(root, root=True))
    stack = [root.left, root.right];

    #i = 2  # position in the hierarchy
    while stack != []:
        node = stack.pop(0)
        #print node, node.branches
        if node.branches:
            # inseatd of saving tree in breadth first manner
            # we are going to save it in dynamic manner,(we won't save empty nodes)
            # current nodes children are in position (len(hierarchy)+len(stack)+1))+1, left child
            # and (len(hierarchy)+len(stack)+1))+2, right child
            hierarchy.append(build_hierarchy2(node, len(hierarchy)+len(stack)))
            stack.append(node.left)
            stack.append(node.right)
        else:
            # if not, it has an empty list
            hierarchy.append(build_hierarchy2(node, len(hierarchy)+len(stack)))

    hierarchy[0]['parent'] = -1;
    for e in xrange(len(hierarchy)):
        print hierarchy[e]
        if hierarchy[e]['children'] != []:
            hierarchy[hierarchy[e]['children'][0]]['parent'] = e;
            hierarchy[hierarchy[e]['children'][1]]['parent'] = e;
            
    hierarchy = json.dumps(hierarchy)
    
    return render(request, 'interactions/cforange_hierarchical_clustering.html', {'widget' : widget, 'hierarchy' : hierarchy, 'attributes':attributes,'vizualization':input_dict['visualization']})


def filter_table(request, input_dict, output_dict, widget):
    from workflows.visualization_views import orng_table_to_dict
    data = input_dict['data']
    return render(request, 'interactions/cforange_filter_table.html', {'widget' : widget,'input_dict' : input_dict,'output_dict' : orng_table_to_dict(data)})
