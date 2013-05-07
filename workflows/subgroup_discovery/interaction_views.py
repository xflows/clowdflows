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

def select_subgroups(request, input_dict, output_dict, widget):
    '''
    Subgroup selection.
        
    @author: Anze Vavpetic, 2012
    '''
    sd_rules = input_dict['rules']
    rules = sd_rules.rules
    P, N = float(len(sd_rules.targetClassRule.TP)), float(len(sd_rules.targetClassRule.FP))
    subgroups = [(rule.id, rule.ruleToString(), len(rule.FP), len(rule.TP)) for rule in rules]
    return render(request, 'interactions/select_subgroups.html', {
        'widget' : widget,
        'model_name' : sd_rules.algorithmName,
        'subgroups' : subgroups
        })

def table_from_sets(request, input_dict, output_dict, widget):
    '''
    Makes a table with the given sets of examples as the classes.

    @author: Anze Vavpetic, 2012
    '''
    tables = input_dict['data']
    if len(tables) == 2:
        class_name = 'group'
        class_values = ['data group', 'control group']
    else:
        class_name = 'class name'
        class_values = ['class%d' % i for i in range(len(tables))]
    return render(request, 'interactions/table_from_sets.html', {
        'widget' : widget,
        'class_name' : class_name,
        'class_values' : class_values,
        'replace' : True
        })
