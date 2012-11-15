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
    import objectPprint as opp
    obj = input_dict['obj']
    maxStringLen = ToInt(input_dict['maxStringLen'])
    objstr = ""
    if input_dict.has_key('attribute') and input_dict['attribute']!="":
        try:
            obj = eval("obj."+input_dict['attribute'])
        except:
            objstr += "Given attribute '" + input_dict['attribute'] + "' can not be resolved. Showing original object instead:\n"
    objstr += opp.ppprint(obj)
    if (len(objstr)>maxStringLen):
        moreChar = len(objstr) - maxStringLen
        objstr = objstr[0:maxStringLen] + "\n... <Additional " + str(moreChar) + " characters were trimmed due to widget settings.>"

    output_dict = {'object_string': objstr}
    return render(request, 'visualizations/advanced_object_viewer.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})

def show_table(request,input_dict,output_dict,widget):
    tbl= input_dict['tbl']
    output_dict = {'attrs':list(tbl.items()[0][0]), 'metas':[], 'data':tbl.values()[0], 'title':'Vocabulary Table'}
    return render(request, 'visualizations/table_viewer.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})