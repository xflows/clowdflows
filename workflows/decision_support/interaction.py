'''
Decision support interaction views.
'''
from django.shortcuts import render

def kepner_tregoe(request, input_dict, output_dict, widget):
    attributes = [att.name for att in input_dict['data'].domain.features]
    return render(request, 'interactions/kepner_tregoe.html', {'widget':widget, 'attributes':attributes})
