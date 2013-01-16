from django.shortcuts import render

def orange_filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/orange_filter_integers.html',{'widget':widget,'intList':input_dict['intList']})