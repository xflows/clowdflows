from django.shortcuts import render

def big_data_filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/big_data_filter_integers.html',{'widget':widget,'intList':input_dict['intList']})