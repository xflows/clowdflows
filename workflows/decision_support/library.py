'''
Decision support library functions.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
def decision_support_wsm(input_dict):
    output_dict = input_dict
    output_dict['model'] = None
    return output_dict

def decision_support_wsm_finished(postdata, input_dict, output_dict):
    from wsm import WeightedSumModel
    # Fetch the data from the form.
    data = input_dict['data']
    attributes = sorted([att.name for att in data.domain.features])
    weights = {}
    widget_id = int(postdata['widget_id'][0])
    minimize = set()
    ranges = {}
    for idx, att in enumerate(attributes):
        weights[att] = int(postdata['weight-%d-%d' % (widget_id, idx)][0])
        direction = postdata['direction-%d-%d' % (widget_id, idx)][0]
        if direction == 'min':
            minimize.add(att)
        lower_bound = float(postdata['rangeMin-%d-%d' % (widget_id, idx)][0])
        upper_bound = float(postdata['rangeMax-%d-%d' % (widget_id, idx)][0])
        ranges[att] = (lower_bound, upper_bound)

    # Instantiate a WeightedSumModel model.
    kt = WeightedSumModel(data, user_weights=weights, minimize=minimize, 
                          ranges=ranges)
    output_dict = {}
    output_dict['data'] = kt()
    output_dict['model'] = kt
    return output_dict

def decision_support_sensitivity_analysis(input_dict):
    return input_dict

def decision_support_charts(input_dict):
    return input_dict
