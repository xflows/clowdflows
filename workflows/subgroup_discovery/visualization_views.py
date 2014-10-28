'''
Subgroup discovery visualization views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render
import json
import colorsys
from SubgroupDiscovery.calcHull import calcRates
from SubgroupDiscovery.SDRule import SDRules
import measures


def subgroup_roc_visualization(request, input_dict, output_dict, widget):
    '''
    Subgroup roc visualization.
        
    @author: Anze Vavpetic, 2012
    '''
    roc_data = []
    n = len(input_dict['rules'])
    colors = ['#%2x%2x%2x' % tuple(map(lambda c: 255 * c, colorsys.hsv_to_rgb(x / float(n), 0.5, 0.8))) for x in range(n)]
    for i, sd_rules in enumerate(input_dict['rules']):
        P, N = float(len(sd_rules.targetClassRule.TP)), float(len(sd_rules.targetClassRule.FP))
        calcRates(sd_rules)
        rule_points = [{'x' : len(rule.FP)/N, 'y' : len(rule.TP)/P, 'name' : rule.ruleToString()} for rule in sd_rules.rules]
        roc_data.append({'type': 'line', 'name' : sd_rules.algorithmName + ' (hull)', 'color' : colors[i], 'zIndex' : 0, 'data' : zip(sd_rules.hullFPR, sd_rules.hullTPR)})
        roc_data.append({'type': 'scatter', 'name' : sd_rules.algorithmName, 'color' : colors[i], 'zIndex' : 1, 'data' : rule_points})
    return render(request, 'visualizations/subgroup_roc_visualization.html', {
        'widget' : widget,
        'roc_data' : json.dumps(roc_data),
        })

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
        'subgroups' : json.dumps(subgroups),
        'rules' : [(i, descr) for i, descr in enumerate(subgroups)]
        })

def subgroup_measures(request, input_dict, output_dict, widget):
    '''
    Subgroup measures calculated for sets of rules.
        
    @author: Anze Vavpetic, 2014
    '''
    header = ['Rule Set'] + [name for name, _ in measures.DISCRETE_TARGET_MEASURES]
    rs_evaluations = []
    for sdrules in input_dict.get('rules', []):
        if isinstance(sdrules, SDRules):
            posEx = len(sdrules.targetClassRule.TP)
            negEx = len(sdrules.targetClassRule.examples) - posEx
            rs_name = sdrules.algorithmName
        else:
            rs_name = rules.get('name', 'No name')
            posEx, negEx = rules['posEx'], rules['negEx']
        rs_evaluations.append([rs_name] + measures.evaluate(posEx, negEx, sdrules.rules))
    return render(request, 'visualizations/subgroup_measures_visualization.html', {
        'widget' : widget,
        'header': header,
        'rs_evaluations': rs_evaluations
        })
