import orange


class WeightedSumModel:
    '''
    Weighted sum decision support model.
    '''
    def __init__(self, data, user_weights=None, minimize=None, ranges=None):
        self.data = data
        self.user_weights = user_weights
        self.minimize = minimize if minimize else set()
        self.ranges = ranges if ranges else self.default_ranges()
        self.unusable = self.unusable_attributes()
        self.labels = self.generate_labels()
        self.name = 'Weighted sum model'

    def default_ranges(self):
        """Calculates the default ranges of the attributes"""
        data_np = self.data.to_numpy()[0]
        ranges = {}
        for i, att in enumerate(self.data.domain.features):
            if att.varType == orange.VarTypes.Continuous:
                ranges[att.name] = (min(data_np[:, i]), max(data_np[:, i]))
        return ranges

    def unusable_attributes(self):
        unusable = []
        for att in self.data.domain.features:
            if att.varType != orange.VarTypes.Continuous:
                unusable.append(att.name)
        return unusable

    def generate_labels(self):
        labels = []
        has_labels = 'label' in self.data.domain
        for idx, ex in enumerate(self.data):
            if has_labels:
                labels.append(ex['label'].value)
            else:
                labels.append(idx)
        return labels

    def label(self, idx):
        return self.labels[idx]

    def __call__(self, weights=None):
        if not weights:
            weights = self.user_weights

        # New augmented table
        norm_data = orange.ExampleTable(self.data)
        newid = min(norm_data.domain.get_metas().keys(), 0) - 1
        score_attr = orange.FloatVariable('score')
        norm_data.domain.add_meta(newid, score_attr)
        norm_data.add_meta_attribute(score_attr)

        # Normalize the attributes to the proper range
        for att, (lower_bound, upper_bound) in self.ranges.items():
            for ex in norm_data:
                ex[att] = ex[att] / (upper_bound - lower_bound)

        # Normalize column-wise
        col_sum = {}
        for att in norm_data.domain.features:
            col_sum[att] = float(sum([ex[att] for ex in norm_data]))
        for ex in norm_data:
            for att in norm_data.domain.features:
                ex[att] = ex[att] / col_sum[att]

        # Use the inverse of an attr. value it should be minimized.
        inverse = lambda x, att: 1 - x if att in self.minimize else x
        for ex in norm_data:
            score = sum([inverse(ex[att].value, att) * weights.get(att, 1)
                        for att in self.ranges.keys()])
            ex['score'] = score

        return norm_data
