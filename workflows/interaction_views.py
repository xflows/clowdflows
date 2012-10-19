from django.shortcuts import render

def test_interaction(request,input_dict,output_dict,widget):
    return render(request, 'interactions/test_interaction.html',{'widget':widget})

def filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/filter_integers.html',{'widget':widget,'integers':input_dict['integers']})
    
def noiserankview(request,input_dict,output_dict,widget):
    dataset = input_dict['data']
    allnoise = output_dict['allnoise']
    return render(request, 'interactions/noise_rank.html',{'widget':widget,'allnoise':allnoise, 'data': dataset})    
    
def select_attrs(request, input_dict, output_dict, widget):
    import orange, Orange
    data = Orange.data.Table(input_dict['data'])
    d = data.domain

    classes = d.class_var.name

    metas = []
    for m in d.getmetas().values():
        metas.append(str(m.name))

    attrs = {}
    for a in d.attributes:
        attrs[a.name] = a.varType    
        
    sorted_attrs = sorted(attrs.items())        

    input_dict = {'ca':classes, 'ma':metas, 'attrs':attrs, 'data':input_dict['data'],'sorted_attrs':sorted_attrs}
    return render(request, 'interactions/select_attrs.html',{'widget':widget, 'input_dict':input_dict})

def select_data(request, input_dict, output_dict, widget):
    
    import Orange

    data = Orange.data.Table(input_dict['data'])

    attrs = {}

    for a in data.domain.variables:
        values = []
        try:
            for v in a.values:
                values.append(v)
        except:
            pass
 
        attrs[a.name] = {'feature':1, 'type':str(a.var_type), 'values':values}

    for a in data.domain.get_metas():
        meta = data.domain.get_meta(a)
        
        values = []
        try:
            for v in meta.values:
                values.append(v)
        except:
            pass

        attrs[meta.name] = {'feature':0, 'type':str(meta.var_type), 'values':values}

    sorted_attrs = sorted(attrs.items())
    input_dict = {'data': data, 'attrs':attrs, 'sorted_attrs':sorted_attrs}
    return render(request, 'interactions/select_data.html',{'widget':widget, 'input_dict':input_dict})

def build_subgroups(request, input_dict, output_dict, widget):
    import Orange

    data = Orange.data.Table(input_dict['data'])

    class_values = []

    for v in data.domain.class_var.values:
        class_values.append(v)

    target = {'name':data.domain.class_var.name, 'values':class_values}

    return render(request, 'interactions/build_subgroups.html', {'widget':widget, 'data':data, 'target':target})
    
def kepner_tregoe(request, input_dict, output_dict, widget):
    attributes = [att.name for att in input_dict['data'].domain.features]
    return render(request, 'interactions/kepner_tregoe.html', {'widget':widget, 'attributes':attributes})

def alter_table(request, input_dict, output_dict, widget):
    from visualization_views import orng_table_to_dict
    data = input_dict['data']
    return render(request, 'interactions/alter_table.html', {'widget' : widget,'input_dict' : input_dict,'output_dict' : orng_table_to_dict(data)})
