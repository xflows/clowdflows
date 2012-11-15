import orange
import sys
from SDRule import *

true = 1
false = 0

class Apriori_SD:
    def __init__(self,  minSupport = 0.05, minConfidence = 0.8, k=3):
        self.minSup = minSupport
        self.minConf = minConfidence
        self.weightID = orange.newmetaid()
        self.k = k

    def __call__(self, data, targetClass, max_rules=0):
        '''Returns the Apriori-C classifier.'''
        
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
        self.rulesSD = []

        # build association classification rules
        rules = orange.AssociationRulesInducer(data, support = self.minSup, classificationRules = 1, maxItemSets =10000000 )

        #_______________________________ post-processing step 1
        # select rules that classify in the target class
        right= orange.Example(data.domain,[orange.Value(orange.VarTypes.Discrete, orange.ValueTypes.DK)]*len(data.domain))
        right.setclass(targetClass)
        rules = rules.filter(lambda rule: rule.right == right) 

        # select rules with confidence >= minConfidence
        rules = rules.filter(lambda rule: rule.confidence >= self.minConf) 

        #________________________________ post processing step 2
        # weighted covering
        self.data.addMetaAttribute(self.weightID)  # set weights of all examples to 1
        bestRuleWRacc = 100
        while len(rules)>0 and self.uncoveredExamples()>0 and bestRuleWRacc > 0 and (max_rules==0 or len(self.rulesSD)<max_rules):
            (bestRule,bestRuleWRacc)= self.findBestRule(rules)
            rules.remove(bestRule)
            self.removeSimilarRules(bestRule, rules)     
            self.decreaseExampleWeights(bestRule)
            self.rulesSD.append(bestRule)
        
        #____________________________ transform rules to SD format
        beam = []        
        targetClassRule = SDRule(data, targetClass, conditions=[], g =1)

        for r in self.rulesSD:
            cond = []
            for i in range(len(r.left)):
                if  not orange.Value.is_DC(r.left[i]):
                    cond.append(orange.ValueFilter_discrete(
                                position = i,
                                values = [orange.Value(data.domain.attributes[i], r.left[i])]))
            rSD = SDRule(data, targetClass, cond)
            beam.append(rSD)
        
        if data_discretized:
            targetClassRule = SDRule(original_data, targetClass, conditions=[], g=1)
            # change beam so the rules apply to original data
            beam = [rule.getUndiscretized(original_data) for rule in beam]
        else:
            targetClassRule = SDRule(data, targetClass, conditions=[], g =1)
        
        return  SDRules(beam, targetClassRule,"Apriori-SD")
        
    def removeSimilarRules(self, rule , rules):
        for r in rules[:]:
            if self.areSimilar(r,rule):
                rules.remove(r)
                
    def areSimilar(self, rule1, rule2):
        b1 = b2 = false
        if rule1.right == rule2.right:     # classify in the same class
            b1 = b2 = true
            for i in range(len(rule1.left)):
                if not(rule1.left[i].is_DC() or rule1.left[i] == rule2.left[i] ):
                    b1 = false
                if not(rule2.left[i].is_DC() or rule1.left[i] == rule2.left[i] ):
                    b2 = false
        return (b1 or b2)

    def findBestRule(self,rules):           # rules should not be empty
        bestRule = rules[0]                     
        bestRuleWRacc = self.wWRAccImp(bestRule)
        for r in rules:
            tmp = self.wWRAccImp(r)
            if tmp > bestRuleWRacc:
                bestRuleWRacc = tmp
                bestRule = r
        return (bestRule,bestRuleWRacc)
            
    def uncoveredExamples(self):
        """Returns the number of examples that have not been covered more than k times."""
        return len(map(lambda d: d.getweight(self.weightID) <= self.k, self.data))
    
    def decreaseExampleWeights(self, rule):
        for d in self.data:
            if (rule.appliesBoth(d)):
                d.setweight(self.weightID, d.getweight(self.weightID) + 1)

    def wWRAccImp(self, rule):
        N  = len(self.data)
        ny = rule.nAppliesRight
        N1 = n1x = n1xy = 0

        for d in self.data:
            if  d.getweight(self.weightID) <= self.k:
                tmp = 1 / (1 + d.getweight(self.weightID) )
                N1 +=  tmp
                if rule.appliesLeft(d):
                    n1x += tmp
                    if rule.appliesBoth(d):
                        n1xy += tmp
        return n1xy/N1 - ny * n1x /(N1 * N)                   





if __name__=="__main__":
    filename = "..\\..\\doc\\datasets\\lenses.tab"
    if 'linux' in sys.platform:
        filename= "/usr/doc/orange/datasets/lenses.tab"
    data = orange.ExampleTable(filename)
    print    
    learner = Apriori_SD(minSupport = 0.3, minConfidence = 0.3, k=4)
    targetClass= orange.Value(data.domain.classVar, "none")
    rules = learner(data,targetClass,0)
    rules.printRules()

    
