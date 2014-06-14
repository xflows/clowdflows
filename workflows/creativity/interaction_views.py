from django.shortcuts import render

def creativity_filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/creativity_filter_integers.html',{'widget':widget,'intList':input_dict['intList']})