'''
Bioinformatics visualization viewes.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render

def rank_plotter(request, input_dict, output_dict, widget):
    #TODO
    return render(request, 'interactions/rank_plotter.html', {'widget':widget})

def segs_rule_browser(request, input_dict, output_dict, widget):
    #TODO
    return render(request, 'interactions/segs_rule_browser.html', {'widget':widget})
