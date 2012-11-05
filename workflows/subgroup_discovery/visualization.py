'''
Subgroup discovery visualization views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render
import json
from SubgroupDiscovery.calcHull import calcRates

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

def subgroup_roc_visualization(request, input_dict, output_dict, widget):
    '''
    Subgroup roc visualization.
        
    @author: Anze Vavpetic, 2012
    '''
    roc_data = []
    for i, sd_rules in enumerate(input_dict['rules']):
        P, N = float(len(sd_rules.targetClassRule.TP)), float(len(sd_rules.targetClassRule.FP))
        calcRates(sd_rules)
        rule_points = [{'x' : len(rule.FP)/N, 'y' : len(rule.TP)/P, 'name' : rule.ruleToString()} for rule in sd_rules.rules]
        roc_data.append({'type': 'line', 'name' : sd_rules.algorithmName, 'data' : zip(sd_rules.hullFPR, sd_rules.hullTPR)})
        #roc_data.append({'type': 'scatter', 'name' : sd_rules.algorithmName, 'data' : rule_points})
    return render(request, 'visualizations/subgroup_roc_visualization.html', {
        'widget' : widget,
        'roc_data' : json.dumps(roc_data),
        })