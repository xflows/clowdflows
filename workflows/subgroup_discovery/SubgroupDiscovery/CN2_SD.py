import orange
import sys
from SDRule import *

true = 1
false = 0

class CN2_SD:
    def __init__(self,k):
        self.k = k
        self.counter = orange.newmetaid()
        self.weightID = orange.newmetaid()
        self.rbf = orange.RuleBeamFinder()
        self.rbf.evaluator = RuleEvaluator_WRAcc()
        
        
        
    def __call__(self, data, targetClass, num_of_rules=0):
        '''Returns CN2-SD rules by performing weighted covering algorithm.'''
        
        data_discretized = False
        # If any of the attributes are continuous, discretize them
        if data.domain.hasContinuousAttributes():
            original_data = data
            data_discretized = True
            new_domain = []
            discretize = orange.EntropyDiscretization(forceAttribute=True)
            for attribute in data.domain.attributes:
                if attribute.varType == orange.VarTypes.Continuous:
                    d_attribute = discretize(attribute, data)
                    # An attribute is irrelevant, if it is discretized into a single interval
#                        if len(d_attribute.getValueFrom.transformer.points) > 0:
                    new_domain.append(d_attribute)
                else:
                    new_domain.append(attribute)
            data = original_data.select(new_domain + [original_data.domain.classVar])
        
        self.data = data
        self.max_rules = num_of_rules
        rules = []

        tc = orange.Value(data.domain.classVar, targetClass)

        # weighted covering
        self.data.addMetaAttribute(self.weightID)  # set weights of all examples to 1
        self.data.addMetaAttribute(self.counter)   # set counters of all examples to 0

        targetClassRule = SDRule(data, targetClass, conditions=[], g =1)

        tmpRule =  self.rbf(data, self.weightID, targetClass, None)
        while (tmpRule.quality>0) and (self.max_rules==0 or len(rules)<self.max_rules):
            bestRule = SDRule(self.data, tc, tmpRule.filter.conditions)
            bestRule.quality = tmpRule.quality
            self.decreaseExampleWeights(bestRule)
            rules.append(bestRule)
            tmpRule =  self.rbf(data, self.weightID, targetClass, None)
            
        if data_discretized:
            targetClassRule = SDRule(original_data, targetClass, conditions=[], g=1)
            # change beam so the rules apply to original data
            rules = [rule.getUndiscretized(original_data) for rule in rules]
        else:
            targetClassRule = SDRule(data, targetClass, conditions=[], g =1)
        
        return  SDRules(rules, targetClassRule, "CN2-SD")
    #________________________________ 

    
    def decreaseExampleWeights(self, rule):
        for d in self.data:            
            if d.getclass()==rule.targetClass:
                tmp = d.getweight(self.counter)+1.0
                if tmp>self.k:
                    d.setweight(self.weightID, 0)
                else:
                    d.setweight(self.weightID, 1.0/(tmp+1))
                d.setweight(self.counter, tmp)


class RuleEvaluator_WRAcc(orange.RuleEvaluator):
    def __call__(self, newRule, examples, weightID, targetClass, prior):
        N  = len(examples)
        ny = len(filter(lambda e: e.getclass()==targetClass, examples))
        N1 = n1x = n1xy = 0

        for e in examples:
            tmp = e.getweight(weightID)
            N1 +=  tmp
            if newRule.filter(e):
                n1x += tmp
                if e.getclass() == targetClass:
                    n1xy += tmp        
        wracc = n1xy/N1 - ny * n1x /(N1 * N)
        return wracc



if __name__=="__main__":
    filename = "..\\..\\doc\\datasets\\lenses.tab"
    if 'linux' in sys.platform:
        filename= "/usr/doc/orange/datasets/lenses.tab"
    data = orange.ExampleTable(filename)

    print
    print
    print    
    learner = CN2_SD(3)
    targetClass = orange.Value(data.domain["lenses"], "none")
    rules = learner(data,targetClass,10)
    print "____________________SN2-SD results______________________"
    rules.printRules()

    
