'''
Decision support visualization views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render
import json

def sensitivity_analysis_viewer(request, input_dict, output_dict, widget):
    '''
    Computes the sensitivity analysis graph.
        
    @author: Anze Vavpeltic, 2012
    '''
    model = input_dict['model']
    attributes = [att.name for att in input_dict['model'].data.domain.features]
    data_points = {}
    domain = range(0, 101, 10)
    # Compute for each attribute
    for target_att in attributes:
        y, ex_data = [], {}
        # For collecting scores for each example across different weights
        for ex in model.data:          
            ex_data[ex['label'].value] = []
        # Compute the scores for each weight
        for w in domain:
            model.weights[target_att] = w
            ds = model()
            for ex in ds:
                ex_data[ex['label'].value].append([w, ex['score'].value])
        for ex in model.data:          
            y.append({'name' : ex['label'].value, 'data' : ex_data[ex['label'].value]})
        data_points[target_att] = y        
       
    return render(request, 'visualizations/sensitivity_analysis.html', 
                  {'widget' : widget,
                   'attributes': attributes, 
                   'data_points' : json.dumps(data_points), 
                   'output_dict': {}
                   })

def ds_charts_viewer(request, input_dict, output_dict, widget):
    '''
    Decision support visualization.
        
    @author: Anze Vavpeltic, 2012
    '''
    model = input_dict['model']
    norm_data = model()
    weight_shares = [ [att, weight] for att, weight in model.weights.items() ]
    attributes = sorted(model.weights.keys())
    alternatives = [ex['label'].value for ex in norm_data]
    weights_bar = [{ 'data' : [model.weights[att] for att in attributes] }]
    values_column = [{ 'data' : [ex['score'].value for ex in norm_data] }]
    alt_data = [{ 'name' : ex['label'].value, 'data' : [ex[att].value for att in attributes] } for ex in norm_data ]
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
