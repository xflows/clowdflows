from django.shortcuts import render

def noise_filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/noise_filter_integers.html',{'widget':widget,'intList':input_dict['intList']})