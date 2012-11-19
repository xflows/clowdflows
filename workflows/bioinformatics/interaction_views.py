'''
Bioinformatics interaction viewes.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render

def insilico_search(request, input_dict, output_dict, widget):
    #TODOl
    return render(request, 'interactions/insilico_search.html', {'widget':widget})
