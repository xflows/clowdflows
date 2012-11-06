'''
Subgroup discovery visualization views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render
import json
import colorsys
from SubgroupDiscovery.calcHull import calcRates

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