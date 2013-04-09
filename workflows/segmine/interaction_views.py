'''
Bioinformatics interaction viewes.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render

def feature_selection(request, input_dict, output_dict, widget):
    #TODO
    return render(request, 'interactions/feature_selection.html', {'widget':widget})

