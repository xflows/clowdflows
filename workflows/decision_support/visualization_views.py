'''
Decision support visualization views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render
import json
from collections import defaultdict

def decision_support_sensitivity_analysis_viewer(request, input_dict, output_dict, widget):
    '''
    Computes the sensitivity analysis graph.
        
    @author: Anze Vavpetic, 2012
    '''
    model = input_dict['model']
    attributes = sorted(model.ranges.keys())
    data_points = {}

    # Compute for each attribute
    for target_att in attributes:
        alt_scores = [] 
        ex_data = defaultdict(list)
        weights = dict(model.user_weights)

        # Compute the scores for each weight
        for w in range(0, 101, 10):
            weights[target_att] = w
            for idx, ex in enumerate(model(weights=weights)):
                label = model.label(idx)
                ex_data[label].append([w, ex['score'].value])

        for idx, ex in enumerate(model.data):
            label = model.label(idx)
            data = ex_data[label]
            alt_scores.append({'name' : label, 'data' : data})
        data_points[target_att] = alt_scores
       
    return render(request, 'visualizations/sensitivity_analysis.html', 
                  {'widget' : widget,
                   'attributes': attributes, 
                   'data_points' : json.dumps(data_points), 
                   'output_dict': {}
                   })

def decision_support_charts_viewer(request, input_dict, output_dict, widget):
    '''
    Decision support visualization.
        
    @author: Anze Vavpetic, 2012
    '''
    model = input_dict['model']
    scores = model()
    weight_shares = model.user_weights.items()
    attributes = sorted(model.ranges.keys())
    alternatives = [model.label(idx) for idx, ex in enumerate(scores)]
    weights_bar = [{'data' : [model.user_weights[att] for att in attributes]}]
    values_column = [{'data' : [ex['score'].value for ex in scores] }]
    alt_data = [{'name' : model.label(idx), 
                 'data' : [ex[att].value for att in attributes]} 
                 for idx, ex in enumerate(scores)]
    return render(request, 'visualizations/ds_charts.html', 
                  {'widget' : widget, 
                   'model_name' : model.name, 
                   'attributes' : json.dumps(attributes),
                   'alternatives' : json.dumps(alternatives),
                   'weight_shares' : json.dumps(weight_shares),
                   'weights_bar' : json.dumps(weights_bar),
                   'values_column' : json.dumps(values_column),
                   'alt_data' : json.dumps(alt_data)
                   })
