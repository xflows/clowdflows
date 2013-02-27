from django.shortcuts import render

def lemmagen_display_summation(request,input_dict,output_dict,widget):
    if sum(input_dict['intList']) == input_dict['sum']:
        check = 'The calculation appears correct.'
    else:
        check = 'The calculation appears incorrect!'
    return render(request, 'visualizations/lemmagen_display_integers.html',{'widget':widget,'input_dict':input_dict, 'output_dict':output_dict, 'check':check})
