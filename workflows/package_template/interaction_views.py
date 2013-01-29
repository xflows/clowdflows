from django.shortcuts import render

def pcktmp_filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/pcktmp_filter_integers.html',{'widget':widget,'intList':input_dict['intList']})