import re

def graphs_create_integers(input_dict):
    intStr = input_dict['intStr']
    intList = []
    for i in re.findall(r'\w+', intStr):
        try:
            intList.append(int(i))
        except:
            pass
    if input_dict['sort'].lower() == "true":
        intList.sort()
    return {'intList':intList}

def graphs_sum_integers(input_dict):
    intList = input_dict['intList']
    return {'sum':sum(intList)}

def graphs_pre_filter_integers(input_dict):
    return input_dict

def graphs_post_filter_integers(postdata,input_dict,output_dict):
    intListOut = postdata['intListOut']
    intList = []
    for i in intListOut:
        try:
            intList.append(int(i))
        except:
            pass
    return {'intList': intList}

def graphs_pre_display_summation(input_dict):
    return {}



###########################################


def graphs_visualize_visjs(input_dict):
    return {}


def graphs_json2networkx(input_dict):
    from json import loads
    from networkx.readwrite import json_graph

    gtext = loads(input_dict['graph'])
    g =  json_graph.node_link_graph(gtext)
    return {'nxgraph': g}