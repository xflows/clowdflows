'''
Subgroup discovery interaction views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render
import json

from library import SubgroupDiscovery

def build_subgroups(request, input_dict, output_dict, widget):
    data = input_dict['data']
    # Algorithms and the needed parameters
    className = data.domain.class_var.name
    classValues = data.domain.class_var.values
    return render(request, 'interactions/build_subgroups.html', 
        {'widget':widget, 
        'algorithms' : SubgroupDiscovery.algorithms.keys(), 
        'parameters_info' : SubgroupDiscovery.parameter_info,
        'all_parameters' : json.dumps(SubgroupDiscovery.parameter_info.keys()), 
        'settings' : json.dumps(SubgroupDiscovery.algorithms), 
        'className' : className, 
        'classValues' : classValues})

def subgroup_bar_visualization(request, input_dict, output_dict, widget):
    '''
    Subgroup bar visualization.
        
    @author: Anze Vavpetic, 2012
    '''
    sd_rules = input_dict['rules']
    rules = sd_rules.rules
    P, N = float(len(sd_rules.targetClassRule.TP)), float(len(sd_rules.targetClassRule.FP))
    fpr = [-len(rule.FP)/N for rule in rules]
    tpr = [len(rule.TP)/P for rule in rules]
    subgroups = [rule.ruleToString() for rule in rules]
    return render(request, 'visualizations/subgroup_bar_visualization.html', {
        'widget' : widget,
        'model_name' : sd_rules.algorithmName,
        'fpr' : json.dumps(fpr),
        'tpr' : json.dumps(tpr),
        'subgroups' : json.dumps(subgroups)
        })