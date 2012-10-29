from django.shortcuts import render

#latino
import latino
import logging

def show_adc(request,input_dict,output_dict,widget):
    logging.info('__show_adc__VIEW__')
    return latino.adcView(request,input_dict,output_dict,widget)

def advanced_object_viewer(request,input_dict,output_dict,widget):
    logging.info('__advanced_object_viewer__VIEW__')
    import objectPprint as opp
    obj = input_dict['object']
    maxStringLen = latino.ToInt(input_dict['maxStringLen'])
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
    logging.info('__show_table__VIEW__')
    return latino.ShowTable(request,input_dict,output_dict,widget)