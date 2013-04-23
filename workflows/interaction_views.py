import sys
from django.shortcuts import render

from workflows import module_importer
def setattr_local(name, value, package):
    setattr(sys.modules[__name__], name, value)
module_importer.import_all_packages_libs("interaction_views",setattr_local)

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
    classes = d.class_var.name if d.class_var else None

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
    for att in data.domain.variables:
        values = att.values if hasattr(att, 'values') else []
        attrs[att.name] = {'feature' : 1, 'type' : str(att.var_type), 
                           'values' : list(values)}

    for att in data.domain.get_metas():
        meta = data.domain.get_meta(att)
        values = meta.values if hasattr(meta, 'values') else []
        attrs[meta.name] = {'feature' : 0, 'type' : str(meta.var_type), 
                            'values' : list(values)}

    cls = data.domain.class_var
    if cls:
        values = cls.values if hasattr(cls, 'values') else []
        attrs[cls.name] = {'feature' : 1, 'type':str(cls.var_type), 
                           'values': list(values)}

    sorted_attrs = sorted(attrs.items())
    return render(request, 'interactions/select_data.html', 
                  {'widget' : widget, 'attrs' : sorted_attrs})
    
def alter_table(request, input_dict, output_dict, widget):
    from visualization_views import orng_table_to_dict
    data = input_dict['data']
    return render(request, 'interactions/alter_table.html', {'widget' : widget,'input_dict' : input_dict,'output_dict' : orng_table_to_dict(data)})

def example_distance(request, input_dict, output_dict, widget):
    return render(request, 'interactions/example_distance.html',{'widget':widget, 'input_dict':input_dict})