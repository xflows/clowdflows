import orange
import sys
from SDRule import *
import Preprocessor

true = 1
false = 0

class Beam_SD_preprocessed:
    def __init__(self, minSupport = 0.2, beamWidth = 5, g = 1, **kwds):
        self.minSupport = minSupport
        self.beamWidth = beamWidth
        self.g = g

    def __call__(self, data, targetClass, num_of_rules ):
        if self.dataOK(data):  # Checks weather targetClass is discrete
            original_data = data
            data = Preprocessor.generateFeatures(data, targetClass)
            
            # initialization of beams
            beam = [SDRule(data=data, targetClass=targetClass,  g=self.g)] * self.beamWidth 
            newBeam = [SDRule(data=data, targetClass=targetClass,  g=self.g)] * self.beamWidth
            worstRuleIndex = 0
        
            improvements = true 
            while improvements:  
                improvements = false
                for rule in beam:
                # for f in features:
                    for feature in data.domain.attributes:
                        newRule = rule.cloneAndAddCondition(feature, 'True')
                        if newRule.support > self.minSupport and self.betterThanWorstRule(newRule, newBeam, worstRuleIndex) and self.isRelevant(newRule, newBeam):
                            worstRuleIndex = self.replaceWorstRule(newRule, newBeam, worstRuleIndex)
                            improvements = true
                beam = newBeam
            
            # perform rule subset selection
            if num_of_rules != 0:
                beam = self.ruleSubsetSelection(beam, num_of_rules, data)

            targetClassRule = SDRule(original_data, targetClass, conditions=[], g=1)
            
            # change beam so the rules apply to original data
            fixedBeam = [rule.getFixed(original_data) for rule in beam]
            
            return SDRules(fixedBeam, targetClassRule)


    def isRelevant(self, newRule, beam):
        for rule in beam:
            if  newRule.isIrrelevant(rule):
                return false
        return true

   
    def betterThanWorstRule(self, newRule, beam, worstRuleIndex):
        if newRule.quality > beam[worstRuleIndex].quality:          # better quality
            return true
        elif newRule.quality == beam[worstRuleIndex].quality and newRule.complexity < beam[worstRuleIndex].complexity:   # same quality and smaller complexity
            return true
        else:
            return false

    def replaceWorstRule(self, rule, beam, worstRuleIndex):
        beam[worstRuleIndex] = rule
        wri = 0
        for i in range(len(beam)):
            if beam[i].quality < beam[wri].quality:
                wri = i
        return wri

    def dataOK(self, data):        
#        if data.domain.hasContinuousAttributes():
#            print "All attributes must be discrete."
#            return false
        if data.domain.classVar.varType != orange.VarTypes.Discrete:
            print "Target Variable must be discrete"%(attr.name)
            return false
        return true

    def ruleSubsetSelection(self, beam, num_of_rules, data):
        SS = []
        c = orange.newmetaid()
        data.addMetaAttribute(c)   #initialize to 1
        if num_of_rules <= len(beam):
            for i in range(num_of_rules):
                best_score = 0
                best_rule_index = 0
                for i in range(len(beam)):  
                    score = 0
                    for d in data:          # calculate sum of weights of examples
                        if beam[i].filter(d):
                            score += 1.0/d.getweight(c)
                    if score>best_score:
                        best_score = score
                        best_rule_index = i
                for d in data:              # increase exampe counter
                    if beam[best_rule_index].filter(d):
                        d.setweight(c, d.getweight(c)+1)
                SS.append(beam[best_rule_index])
                del beam[best_rule_index]
            data.removeMetaAttribute(c) 
        return SS
        
#___________________________________________________________________________________
if __name__=="__main__":
    filename = "..\\..\\doc\\datasets\\lenses.tab"
    if 'linux' in sys.platform:
        filename= "/usr/doc/orange/datasets/lenses.tab"
    data = orange.ExampleTable(filename)
        
    learner = Beam_SD_preprocessed(  minSupport = 0.2, beamWidth = 5, g = 6)
    targetClass= orange.Value(data.domain.classVar, "soft")
    beam = learner (data , targetClass=targetClass, num_of_rules=3)
    beam.printRules()
    
