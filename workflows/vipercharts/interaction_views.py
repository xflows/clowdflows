from django.shortcuts import render

def vipercharts_filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/vipercharts_filter_integers.html',{'widget':widget,'intList':input_dict['intList']})
	
def class_from_odt_interactive(request,input_dict,output_dict,widget):
	classes = input_dict['data'].domain.class_var.values
	target = output_dict['target']
	return render(request, 'interactions/class_selection.html',{'widget':widget,'classes':classes, 'target':target})    