'''
Segmine visualization viewes.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render

def segmine_rank_plotter(request, input_dict, output_dict, widget):
    #TODO
    return render(request, 'interactions/segmine_rank_plotter.html', {'widget':widget})

def segmine_rule_browser(request, input_dict, output_dict, widget):
    #TODO
    return render(request, 'interactions/segmine_rule_browser.html', {'widget':widget})
