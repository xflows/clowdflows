'''
Segmine interaction viewes.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render

def segmine_ttest_gene_filter(request, input_dict, output_dict, widget):
    dataset = input_dict['dataset']
    targets = dataset.domain.classVar.values
    return render(request, 'interactions/segmine_ttest_gene_filter.html', 
                  {'widget':widget, 'targets' : targets, 
                   'default_threshold' : 0.05})

def segmine_fc_gene_filter(request, input_dict, output_dict, widget):
    dataset = input_dict['dataset']
    targets = dataset.domain.classVar.values
    return render(request, 'interactions/segmine_fc_gene_filter.html', 
                  {'widget':widget, 'targets' : targets, 
                   'default_threshold' : 1.0})

def segmine_rule_browser(request, input_dict, output_dict, widget):
    rules = input_dict['rules']
    return render(request, 'interactions/segmine_rule_browser.html', 
                  {'widget':widget, 'rules' : rules})

