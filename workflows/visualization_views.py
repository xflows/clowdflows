import sys
from django.shortcuts import render
from django.http import Http404, HttpResponse

from workflows import module_importer
def setattr_local(name, value, package):
    setattr(sys.modules[__name__], name, value)
module_importer.import_all_packages_libs("visualization_views",setattr_local)

def odt_to_tab(request,input_dict,output_dict,widget):
    import Orange
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir
    destination = MEDIA_ROOT+'/'+str(request.user.id)+'/'+str(widget.id)+'.tab'
    ensure_dir(destination)
    input_dict['data'].save(destination)
    filename = str(request.user.id)+'/'+str(widget.id)+'.tab'
    output_dict['filename'] = filename
    return render(request, 'visualizations/string_to_file.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
    
def odt_to_csv(request,input_dict,output_dict,widget):
    import Orange
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir
    destination = MEDIA_ROOT+'/'+str(request.user.id)+'/'+str(widget.id)+'.csv'
    ensure_dir(destination)
    input_dict['data'].save(destination)
    filename = str(request.user.id)+'/'+str(widget.id)+'.csv'
    output_dict['filename'] = filename
    return render(request, 'visualizations/string_to_file.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})    
    
def odt_to_arff(request,input_dict,output_dict,widget):
    import Orange
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir
    destination = MEDIA_ROOT+'/'+str(request.user.id)+'/'+str(widget.id)+'.arff'
    ensure_dir(destination)
    input_dict['data'].save(destination)
    filename = str(request.user.id)+'/'+str(widget.id)+'.arff'
    output_dict['filename'] = filename
    return render(request, 'visualizations/string_to_file.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})

def string_to_file(request,input_dict,output_dict,widget):
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir
    destination = MEDIA_ROOT+'/'+str(request.user.id)+'/'+str(widget.id)+'.txt'
    ensure_dir(destination)
    f = open(destination,'w')
    f.write(str(input_dict['string']))
    f.close()
    filename = str(request.user.id)+'/'+str(widget.id)+'.txt'
    output_dict['filename'] = filename
    return render(request, 'visualizations/string_to_file.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})

def display_string(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/display_string.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})

def object_viewer(request,input_dict,output_dict,widget):
    import pprint
    output_dict = {'object_string':pprint.pformat(input_dict['object'])}
    return render(request, 'visualizations/object_viewer.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})

def orng_table_to_dict(data):
    import Orange
    attrs, metas, data_new = [], [], []
    try:
        class_var = data.domain.class_var.name
    except:
        class_var = ''
    for m in data.domain.get_metas():
        metas.append(data.domain.get_meta(m).name)
    for a in data.domain.attributes:
        attrs.append(a.name)
    pretty_float = lambda x, a: '%.3f' % x if a.var_type == Orange.feature.Type.Continuous else x
    for inst in xrange(len(data)):
        inst_new = []
        for a in data.domain.variables:
            value = data[inst][a.name].value
            inst_new.append((a.name, pretty_float(value, a)))
        for m in data.domain.get_metas():
            value = data[inst][m].value
            a = data.domain.get_meta(m)
            inst_new.append((a.name, pretty_float(value, a)))
        data_new.append(inst_new)
    return {'attrs':attrs, 'metas':metas, 'data':data_new, 'class_var':class_var}

def table_viewer(request,input_dict,output_dict,widget):
    data = input_dict['data']
    return render(request, 'visualizations/table_viewer.html',{'widget':widget,'input_dict':input_dict,'output_dict':orng_table_to_dict(data)})
    
def pr_space_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/pr_space.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
    
def eval_bar_chart_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/eval_bar_chart.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
    
def eval_to_table_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/eval_to_table.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})

def data_table_view(request,input_dict,output_dict,widget):
    #import orange
    data = input_dict['data']
    if data == None:
        view_dict = {'attributes':[], 'examples': []}
        return render(request, 'visualizations/data_table.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict,'view_dict':view_dict})
    else:
        attrs = [x[1].name for x in data.domain.getmetas().items()]
        attrs.extend([x.name for x in data.domain])
        view_dict = {'attributes': attrs}
        view_dict['examples'] = []
        for ex in data:
            newex = []
            for meta in ex.getmetas().items():
                print meta[1].variable.name, ex[meta[1].variable.name]
                newex.append(ex[meta[1].variable.name])
            for x in ex:
                newex.append(x)
            view_dict['examples'].append(newex)    
        return render(request, 'visualizations/data_table.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict,'view_dict':view_dict})
    
def data_info_view(request,input_dict,output_dict,widget):
    import orange
    data = input_dict['data']
    n = len(data)    
    info_dict = {'instances': len(data),
                 'attrs': len(data.domain.attributes),
                 'num_classes' : len(data.domain.classVar.values),
                 'class_type': data.domain.classVar.varType,
                 'has_miss_values': ('yes' if data.hasMissingValues() == 1 else 'no'),
                 'has_miss_classes' : ('yes' if data.hasMissingClasses() == 1 else 'no'),
                 'metas': data.domain.getmetas(),
                 'has_metas': len(data.domain.getmetas())
                 }#                 'classes': data.domain.classVar.values,
    # count number of continuous and discrete attributes
    ncont=0; ndisc=0; 
    for a in data.domain.attributes:
        if a.varType == orange.VarTypes.Discrete:
            ndisc +=  1
        else:
            ncont +=  1
    
    info_dict['discrete'] = ndisc
    info_dict['continuous'] = ncont
    
    # number of instances with missing values
    miss_val_insts =  []
    miss_val_attrs =  [] 
    if info_dict['has_miss_values'] == 'yes':
        for i in range(n):
            for j in range(len(data[i])):
                if data[i][j].isSpecial():
                    if i not in miss_val_insts:
                        miss_val_insts.append(i)
                    if j not in miss_val_attrs:
                        miss_val_attrs.append(j)
                    
    info_dict['miss_val_insts'] = len(miss_val_insts)
    info_dict['mvir'] = len(miss_val_insts)*100./n
    info_dict['miss_val_attrs'] = len(miss_val_attrs)
    info_dict['mvar'] = len(miss_val_attrs)*100./n
    
    # obtain class distribution
    c = [0] * len(data.domain.classVar.values)
    for e in data:
        c[int(e.getclass())] += 1
    r = [0.] * len(c)
    for i in range(len(c)):
        r[i] = c[i]*100./len(data)
    
    info_dict['class_distr'] = [{'class': z[0], 'instances':z[1], 'ratio':z[2]} for z in zip(data.domain.classVar.values, c, r)]
      
    
    return render(request, 'visualizations/data_table_info.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict, 'info_dict':info_dict})

def sdmsegs_viewer(request,input_dict,output_dict,widget):
    import json
    d = json.loads(input_dict['json'])
    output = {}
    for k, v in d['A']['WRAcc'].items():
        terms = []
        for t in v['terms']:
            try:
                terms.append(d['ontDict'][t])
            except Exception, e:
                pass
        output[int(k)] = {
        'name': terms,
        'topGenes': int(len(v['topGenes'])),
        'allGenes': int(len(v['allGenes'])),
        'wracc': round(v['scores']['wracc'], 3)
        }
    output_dict = {'json_output':output}
    return render(request, 'visualizations/sdmsegs_viewer.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})

def treeToJSON(node, path="", nodes={}):
    #made by Bogdan Okresa Duric :)
    
    import Orange
    import json

    if not node:
        return

    if path=="": #get the dictionary prepared, insert root node
        nodes.update ({ #root node properties
            "name":node.branch_selector.class_var.name,
            "ID":node.reference(),
            "children":[]
        })
        path = "['children']" #prepare path for future use, it points into 'children' property of the root node

    if node.branch_selector: #if the node has branches
        for n in node.branches: #walk through all the branches one by one
            try:
                if n.branch_selector: #if the node (branch) has branches
                    child = { #set node properties
                        "name":n.branch_selector.class_var.name,
                        "ID":n.reference(),
                        "children":[] #stays open for future descendant nodes
                        }

                    eval ("nodes" + path + ".append(" + str(child) + ")") #write node properties
                    # 'nodes' is the dictionary
                    #path is the path to the current node, i.e. current parent node
                    #child is dictionary with node properties

                else: #if node is a leaf
                    child = {
                        "name":n.node_classifier.default_value.value,
                        "ID":n.reference(),
                        }
                    eval ("nodes" + path + ".append(" + str(child) + ")")
            except:
                pass
        for i in range(len(node.branches)): #go and work with the branches, one by one
            treeToJSON(node.branches[i], path + "[" + str(i) + "]" + "['children']", nodes) #work with child node, adding it's "address"
    else: #if the node has no branches, simply return
        return

    return json.JSONEncoder().encode(nodes) #output complete JSON description of the tree

def tree_visualization(request, input_dict, output_dict, widget):
    import Orange
    import json

    tc = input_dict['clt']

    print "viz start"

    # jsonT = treeToJSON(tc.tree)
    jsonJ = treeToJSON(tc.tree)

    print "viz end"

    # jsonJ = json.dumps(jsonT, separators=(',',':'))
    # jsonJ = json.JSONEncoder().encode(jsonT)

    print jsonJ

    # import tempfile
    # f = tempfile.NamedTemporaryFile(delete=False,suffix='.json')

    # f.write(jsonJ)

    # f.close()

    # print f.name
    
    return render(request, 'visualizations/tree_visualization.html', {'widget':widget, 'input_dict':input_dict, 'json':jsonJ})