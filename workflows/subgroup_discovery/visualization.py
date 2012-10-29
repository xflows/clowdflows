'''
Subgroup discovery visualization views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render

def subgroup_visualization(request, input_dict, output_dict, widget):
    '''
    Subgroup visualizations.
        
    @author: Anze Vavpeltic, 2012
    '''

    return render(request, 'visualizations/subgroup_visualization.html', {})