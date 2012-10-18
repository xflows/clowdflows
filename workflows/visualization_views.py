from django.shortcuts import render
from django.http import Http404, HttpResponse
import nlp
import json

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
    
def table_viewer(request,input_dict,output_dict,widget):
    import Orange

    data = input_dict['data']

    attrs = []
    metas = []
    data_new = []

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
            inst_new.append(pretty_float(value, a))
        for m in data.domain.get_metas():
            value = data[inst][m].value
            a = data.domain.get_meta(m)
            inst_new.append(pretty_float(value, a))
        data_new.append(inst_new)

    output_dict = {'attrs':attrs, 'metas':metas, 'data':data_new, 'class_var':class_var}

    return render(request, 'visualizations/table_viewer.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
    
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


def definition_sentences_viewer(request, input_dict, output_dict, widget):
    """
    Parses the input XML and displays the definition sentences given as input.
    
    @author: Anze Vavpetic, 2012
    """
    sentences = nlp.parse_def_sentences(input_dict['candidates']) 
    return render(request, 'visualizations/def_sentences.html',{'widget' : widget, 'sentences' : sentences})


def term_candidate_viewer(request, input_dict, output_dict, widget):
    """
    Parses the input and displays the term candidates.
    
    @author: Anze Vavpeltic, 2012
    """
    terms = []
    for line in input_dict['candidates'].split('\n'):
        try:
            score, cand, lemma = line.split('\t')
        except:
            continue
        terms.append({'score' : score, 
                      'cand' : cand.replace('[', '').replace(']',''),
                      #'lemma' : lemma.replace('<<', '').replace('>>','')
                      })
    terms = sorted(terms, key = lambda x: x['score'], reverse=True)
    return render(request, 'visualizations/terms.html', {'widget' : widget, 'terms' : terms})

def sensitivity_analysis_viewer(request, input_dict, output_dict, widget):
    '''
    Computes the sensitivity analysis graph.
        
    @author: Anze Vavpeltic, 2012
    '''
    model = input_dict['model']
    attributes = [att.name for att in input_dict['model'].data.domain.features]
    data_points = {}
    domain = range(0, 101, 10)
    # Compute for each attribute
    for target_att in attributes:
        y, ex_data = [], {}
        # For collecting scores for each example across different weights
        for ex in model.data:          
            ex_data[ex['label'].value] = []
        # Compute the scores for each weight
        for w in domain:
            model.weights[target_att] = w
            ds = model()
            for ex in ds:
                ex_data[ex['label'].value].append([w, ex['score'].value])
        for ex in model.data:          
            y.append({'name' : ex['label'].value, 'data' : ex_data[ex['label'].value]})
        data_points[target_att] = y        
       
    return render(request, 'visualizations/sensitivity_analysis.html', 
                  {'widget' : widget,
                   'attributes': attributes, 
                   'data_points' : json.dumps(data_points), 
                   'output_dict': {}
                   })

def ds_charts_viewer(request, input_dict, output_dict, widget):
    model = input_dict['model']
    norm_data = model()
    weight_shares = [ [att, weight] for att, weight in model.weights.items() ]
    attributes = sorted(model.weights.keys())
    alternatives = [ex['label'].value for ex in norm_data]
    weights_bar = [{ 'data' : [model.weights[att] for att in attributes] }]
    values_column = [{ 'data' : [ex['score'].value for ex in norm_data] }]
    alt_data = [{ 'name' : ex['label'].value, 'data' : [ex[att].value for att in attributes] } for ex in norm_data ]
    return render(request, 'visualizations/ds_charts.html', 
                  {'widget' : widget, 
                   'model_name' : model.name, 
                   'attributes' : json.dumps(attributes),
                   'alternatives' : json.dumps(alternatives),
                   'weight_shares' : json.dumps(weight_shares),
                   'weights_bar' : json.dumps(weights_bar),
                   'values_column' : json.dumps(values_column),
                   'alt_data' : json.dumps(alt_data)
                   })
