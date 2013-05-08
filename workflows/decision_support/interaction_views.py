'''
Decision support interaction views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render

def decision_support_wsm(request, input_dict, output_dict, widget):
    attributes = [att.name for att in input_dict['data'].domain.features]
    return render(request, 'interactions/wsm.html', {'widget':widget, 'attributes':attributes})
