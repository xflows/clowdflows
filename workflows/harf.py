import orange, orngRFCons

class HarfLearner(orngRFCons.RandomForestLearner):
    def __new__(cls, examples=None, agrLevel = 70, **kwds):
        self = orngRFCons.RandomForestLearner.__new__(cls, **kwds)
        if examples:
            self.__init__(**kwds)
            return self.__call__(examples, weight)
        else:
            return self

    def __init__(self, learner=None, trees=500, attributes=None, name='HARF', rand=None, callback=None):
        """random forest learner"""
        self.trees = trees
        self.name = name
        self.learner = learner
        self.attributes = attributes
        self.callback = callback
    
    def __init__(self, input_str):
        self.learner


#class HarfClassifier(orngRFCons.RandomForestClassifier):
