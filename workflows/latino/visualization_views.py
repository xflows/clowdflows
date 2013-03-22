from django.shortcuts import render
from serialization_utils import *
import django.template.loader

def show_adc(request,input_dict,output_dict,widget):
    view = django.shortcuts.render(request, 'visualizations/adc.html', {'widget': widget,
                                                                        'input_dict': input_dict,
                                                                        'output_dict': output_dict})
    return view

def show_clusters(request,input_dict,output_dict,widget):
    return {}

def show_classifications(request,input_dict,output_dict,widget):
    return {}

def advanced_object_viewer(request,input_dict,output_dict,widget):
    from library_manual import advanced_object_converter
    output_dict = {'object_string': advanced_object_converter(input_dict)["objStr"]}
    return render(request, 'visualizations/advanced_object_viewer.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})

def show_table(request,input_dict,output_dict,widget):
    tbl= input_dict['tbl']
    output_dict = {'attrs':list(tbl.items()[0][0]), 'metas':[], 'data':(((None, v) for v in inst) for inst in tbl.values()[0]), 'title':'Vocabulary Table'}
    return render(request, 'visualizations/table_viewer.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})