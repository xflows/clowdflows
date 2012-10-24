'''
Decision support library functions.
'''

def kepner_tregoe(input_dict):
    output_dict = input_dict
    output_dict['model'] = None
    return output_dict

class KepnerTregoe:
    '''
    Kepner-Tregoe model.
    '''
    def __init__(self, data, weights, smaller_is_better=None):
        self.data = data
        self.weights = weights
        self.smaller_is_better = smaller_is_better if smaller_is_better else set()
        self.name = 'Kepner-Tregoe'
    def __call__(self, weights=None):
        import Orange
        from Orange.feature import Type
        if weights == None:
            weights = self.weights
        # New augmented table
        norm_data = Orange.data.Table(self.data)
        newid = min(norm_data.domain.get_metas().keys()) - 1
        score_attr = Orange.feature.Continuous('score')
        norm_data.domain.add_meta(newid, score_attr)
        norm_data.add_meta_attribute(score_attr)
        # Normalize the attributes column-wise
        for att in norm_data.domain:
            if att.var_type == Type.Continuous:
                col = [ex[att] for ex in norm_data]
                col_norm = float(sum(col))
            for ex in norm_data:
                if att.var_type == Type.Continuous:
                    ex[att] = ex[att] / col_norm 
        # Use the inverse of an attr. value if smaller values should be treated as 'better'.
        inverse = lambda x, att: 1-x if att in self.smaller_is_better else x
        for ex in norm_data:
            score = sum([inverse(ex[att], att.name) * weights.get(att.name, 1) for att in norm_data.domain.features if att.var_type == Type.Continuous])
            ex['score'] = score
        return norm_data

def kepner_tregoe_finished(postdata, input_dict, output_dict):
    # Fetch the data and the weights from the form.
    data = input_dict['data']
    attributes = [att.name for att in data.domain.features]
    weights = {}
    widget_id = postdata['widget_id'][0]
    smaller_is_better = set()
    for att in attributes:
        weights[att]=int(postdata['weight'+str(widget_id)+str(att)][0])
        if postdata.has_key('smallerIsBetter'+str(widget_id)+str(att)):
            smaller_is_better.add(att)
    # Instantiate a KepnerTregoe model.
    kt = KepnerTregoe(data, weights, smaller_is_better=smaller_is_better)
    output_dict = {}
    output_dict['data'] = kt()
    output_dict['model'] = kt
    return output_dict

def sensitivity_analysis(input_dict):
    return input_dict

def ds_charts(input_dict):
    return input_dict
