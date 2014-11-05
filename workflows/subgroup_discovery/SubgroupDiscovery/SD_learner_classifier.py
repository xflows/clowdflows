import sys
import orange
from SDRule import *
from Beam_SD import *
from Beam_SD_preprocessed import *
from Apriori_SD import *
from CN2_SD import *
from xmlMaker import *
import cStringIO



class SD_learner(orange.Learner):
    #static variables
    def_name = 'SD classifier'
    def_alg = 'SD'
    def_minSupport = 0.1
    def_minConfidence = 0.7
    def_beamWidth = 5
    def_g = 1.0
    def_k = 4
    def_maxRules = 0

    def __new__(cls, examples=None, **kwds):

        learner = orange.Learner.__new__(cls, **kwds)
        if examples:
            return learner(examples)
        else:
            return learner

    def __init__(self, name = def_name, algorithm = def_alg, minSupport = def_minSupport, \
                 minConfidence = def_minConfidence, beamWidth = def_beamWidth, g = def_g, \
                 k = def_k, max_rules = def_maxRules):

        # parameter checking
        if algorithm not in ["SD","SD-Preprocess","Apriori-SD","CN2-SD"]:
            raise Exception('unknown algorithm %s.' % algorithm)
        if type(minSupport) is not float or (minSupport > 1.0) or (minSupport <= 0.0):
            raise  ValueError('minSupport should be a float in the (0,1] range.')
        if type(minConfidence) is not float or (minConfidence > 1.0) or (minConfidence <= 0.0):
            raise  ValueError('minConfidence should be a float in the (0,1] range.')
        if type(beamWidth) is not int or (beamWidth <= 0) or (beamWidth > 1000):
            raise  ValueError('beamWidth should be an int in the (0,1000] range.')
        if type(g) not in [int, float] or (g < 0):
            raise  ValueError('g should be a non-negative int or float.')
        if type(k) is not int or (k < 0):
            raise  ValueError('k should be a non-negative int.')
        if type(max_rules) is not int or (max_rules < 0):
            raise  ValueError('max_rules should be a non-negative int.')

        self.name = name
        self.max_rules = max_rules
        self.algorithm = algorithm
        if algorithm == "SD":
            self.learner = Beam_SD(  minSupport , beamWidth , g )
        elif algorithm == "SD-Preprocess":
            self.learner = Beam_SD_preprocessed(  minSupport , beamWidth , g )
        elif algorithm == "Apriori-SD":
            self.learner = Apriori_SD(minSupport , minConfidence , k)
        elif algorithm == "CN2-SD":
            self.learner = CN2_SD( k )
        else:
            raise Exception('No such algorithm %s' % algorithm)

    def __call__(self, learndata, testdata = None, weight = None):
        # because of preprocessing
        if testdata:
            classifier = SD_Classifier(testdata)
        else:
            classifier = SD_Classifier(learndata)

        for targetClassValue in learndata.domain.classVar.values:
            targetClass = orange.Value(learndata.domain.classVar, targetClassValue)
            beam = self.learner (learndata, targetClass, self.max_rules)
            classifier.addRulesForClass(beam, targetClass)

        classifier.name = self.name
        classifier.algorithm = self.algorithm
        return classifier

#________________________________________________________________________________________

class SD_Classifier(orange.Classifier):
    def __init__(self, data=None):
        if data:
            if type(data) is not orange.ExampleTable:
                raise TypeError('Data is not an orange.ExampleTable')
            if data.domain.classVar.varType != orange.VarTypes.Discrete:
                raise TypeError('Data should have a discrete target variable.')

        self.data = data
        self.majorityClassifier = data and orange.MajorityLearner(self.data)
        self.rulesClass = []             # list of istances SDRules
        self.algorithm = "Subgroup discovery algorithm"

    def __call__(self, example, resultType = orange.GetValue):
        # 1. calculate sum of distributions of examples that cover the example
        num_cover = 0.0
        distribution = [0]* len(self.data.domain.classVar.values)
        for rsc in self.rulesClass:
            for rule in rsc.rules.rules:
                if rule.covers(example):
                    num_cover += 1
                    tmp_dist = rule(example, orange.GetProbabilities)
                    for i in range(len(distribution)):
                        distribution[i] += tmp_dist[i]
        # 2. calculate average of distributions of rules that cover example
        if num_cover != 0:
            max_index = 0
            for i in range(len(distribution)):
                distribution[i] = distribution[i]/num_cover
                if distribution[i] > distribution[max_index]:
                    max_index = i
            dist = orange.DiscDistribution(distribution)
            value = orange.Value(self.data.domain.classVar ,self.data.domain.classVar.values[max_index])
        # if no rule fiers
        else:
            value,dist = self.majorityClassifier(example, orange.GetBoth)

        # 3. -----------return
        if resultType == orange.GetValue :
            return value
        elif resultType == orange.GetBoth :
            return (value, dist)
        else :
            return dist

    def addRulesForClass(self, listOfRules, targetClass):
        targetClassRule = SDRule(self.data, targetClass, conditions=[], g =1)
        tmp = SDRules(listOfRules, targetClassRule )
        self.rulesClass.append( tmp)

    def printAll(self):
        rulesList = []
        for rClass in self.rulesClass:
            for rule in rClass.rules.rules:
                rulesList.append(rule.printRule())

        strobj = cStringIO.StringIO()
        strobj.writelines(rulesList)
        rules = strobj.getvalue()
        strobj.close()
        return rules


    def getRules(self, targetClass):
        for rsc in self.rulesClass:
            if rsc.targetClassRule.targetClass == targetClass:
                return rsc.rules

    # The parameter must be a writable object type that supports at least write(string) function
    # e.g. file, StringIO.StringIO, or a custom object type with the write(string) function
    # The resulting object must be closed manually by calling close()
    #
    def toPMML(self, outputObjType=cStringIO.StringIO):
        ''' Ouutput the ruleset in a PMML RuleSet XML schema. '''
        output = outputObjType
        myXML = XMLCreator()
        myXML.DOMTreeTop = dom.DOMImplementation().createDocument('', "PMML", None)
        myXML.DOMTreeRoot = myXML.DOMTreeTop.documentElement
        myXML.DOMTreeRoot.setAttribute("version", "3.2")
        myXML.DOMTreeRoot.setAttribute("xmlns", "http://www.dmg.org/PMML-3_2")
        myXML.DOMTreeRoot.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")

#Header
        header = myXML.insertNewNode(myXML.DOMTreeRoot, "Header")
        header.setAttribute("copyright", "MyCopyright")

        application = myXML.insertNewNode(header, "Application")
        application.setAttribute("name", "Subgroup discovery toolbox for Orange")
        application.setAttribute("version", "1.0")

#DataDictionary
        dataDictionary = myXML.insertNewNode(myXML.DOMTreeRoot, "DataDictionary")
        dataDictionary.setAttribute("numberOfFields", "%d"%len(self.data.domain.variables))
        for var in self.data.domain.variables:
            dataField = myXML.insertNewNode(dataDictionary, "DataField")
            dataField.setAttribute("name", var.name)
            dataField.setAttribute("displayName", var.name)
            dataField.setAttribute("optype", ["","categorical","continuous","other"][var.varType])
            dataField.setAttribute("dataType", ["","string","double",""][var.varType])
            if var.varType==1:
                for val in var.values:
                    value = myXML.insertNewNode(dataField, "Value")
                    value.setAttribute("value", val)
                    value.setAttribute("property", "valid")

#RuleSetModel
        ruleSetModel = myXML.insertNewNode(myXML.DOMTreeRoot, "RuleSetModel")
        ruleSetModel.setAttribute("modelName", "SubgroupDiscoveryRules") # spremeni v dinamicno
        ruleSetModel.setAttribute("functionName", "classification")
        ruleSetModel.setAttribute("algorithmName", self.algorithm)
  # MiningSchema
        miningSchema = myXML.insertNewNode(ruleSetModel, "MiningSchema")
        miningField = myXML.insertNewNode(miningSchema, "MiningField")
        miningField.setAttribute("name", self.data.domain.classVar.name)  # the target variable
        miningField.setAttribute("usageType", "predicted")
        for attr in self.data.domain.attributes:                          # the attributes
            miningField = myXML.insertNewNode(miningSchema, "MiningField")
            miningField.setAttribute("name", attr.name)
            miningField.setAttribute("usageType", "active")

  #RuleSet
        # default rule
        defVal = self.majorityClassifier.defaultValue.value
        defconf = self.majorityClassifier.defaultDistribution [self.majorityClassifier.defaultVal]
        defnbCorrect = defconf * len(self.data)
        ruleSet = myXML.insertNewNode(ruleSetModel, "RuleSet")
        ruleSet.setAttribute("defaultScore", defVal)
        ruleSet.setAttribute("recordCount", "%d"%len(self.data))
        ruleSet.setAttribute("nbCorrect", "%0.0f"%defnbCorrect)
        ruleSet.setAttribute("defaultConfidence", "%0.2f"%defconf)

        ruleSelectionMethod = myXML.insertNewNode(ruleSet, "RuleSelectionMethod")
        ruleSelectionMethod.setAttribute("criterion", "weightedSum")
        #rules
        for rsc in self.rulesClass:                          # for each class
            for i in range(len(rsc.rules.rules)):            # for each rule
                rule = rsc.rules.rules[i]
                simpleRule = myXML.insertNewNode(ruleSet, "SimpleRule")
                simpleRule.setAttribute("id", "%s%d"%(rsc.targetClassRule.targetClass,i+1))
                simpleRule.setAttribute("score", rule.targetClass.value)
                simpleRule.setAttribute("recordCount", "%d"%len(rule.examples))
                simpleRule.setAttribute("nbCorrect", "%d"%len(rule.TP))
                simpleRule.setAttribute("confidence", "%2.2f"%rule.confidence)
                simpleRule.setAttribute("weight", "1")
                if len (rule.filter.conditions )>1  :
                    compoundPredicate = myXML.insertNewNode(simpleRule, "CompoundPredicate")
                    compoundPredicate.setAttribute("booleanOperator", "and")
                elif len (rule.filter.conditions )==1 and \
                     (rule.data.domain[rule.filter.conditions[0].position].varType == orange.VarTypes.Continuous) and \
                      rule.filter.conditions[0].min != float(-infinity):
                    #if there is only one continuous condition in the filter with a range interval
                    compoundPredicate = myXML.insertNewNode(simpleRule, "CompoundPredicate")
                    compoundPredicate.setAttribute("booleanOperator", "and")
                else:
                    compoundPredicate = simpleRule
                for i,c in enumerate(rule.filter.conditions):
                    simplePredicate = myXML.insertNewNode(compoundPredicate, "SimplePredicate")
                    simplePredicate.setAttribute("field", rule.data.domain[c.position].name)
                    if rule.data.domain[c.position].varType == orange.VarTypes.Discrete:
                        simplePredicate.setAttribute("operator", "equal")
                        simplePredicate.setAttribute("value",str(rule.data.domain[c.position].values[int(c.values[0])]))

                    elif rule.data.domain[c.position].varType == orange.VarTypes.Continuous:
                        if c.min == float(-infinity):
                            if not c.outside:
                                simplePredicate.setAttribute("operator", "lessOrEqual") # <=
                            else:
                                simplePredicate.setAttribute("operator", "greaterThan") # >
                            simplePredicate.setAttribute("value","%.3f" % c.max)
                        else:    #interval gets transformed into two simple predicates, one <= and one >
                            simplePredicate.setAttribute("operator", "greaterThan") #this causes problems if there is only one condition with an interval in one rule, sice the "compoundRule" is not present
                            simplePredicate.setAttribute("value","%.3f" % c.min)
                            simplePredicate = myXML.insertNewNode(compoundPredicate, "SimplePredicate")
                            simplePredicate.setAttribute("field", rule.data.domain[c.position].name)
                            simplePredicate.setAttribute("operator", "lessOrEqual")
                            simplePredicate.setAttribute("value","%.3f" % c.max)
                for i in range(len(rule.targetClass.variable.values)):
                    scoreDistribution = myXML.insertNewNode(simpleRule, "ScoreDistribution")
                    scoreDistribution.setAttribute("value", rule.targetClass.variable.values[i])
                    scoreDistribution.setAttribute("recordCount", "%0.0f"%(rule.classDistribution[i]*len(rule.examples)))

        try:
            #PrettyPrint(myXML.DOMTreeTop, output)
            output.write(myXML.DOMTreeTop.toprettyxml())
            return output
        except Exception, e:
            print 'Error while outputting rules in PMML: ' + str(e)
            return None

    #end toPMML()


#___________________________________________________________________________________
if __name__=="__main__":

    # filename = "..\\..\\doc\\datasets\\lenses.tab"
    # if 'linux' in sys.platform:
    #     filename= "/usr/doc/orange/datasets/lenses.tab"
    data = orange.ExampleTable('lenses')


    learner2 = SD_learner(algorithm = "Apriori-SD", minSupport = 0.1, minConfidence= 0.6)
    classifier2 = learner2(data)
    classifier2.printAll()

    learner3 = SD_learner(algorithm = "CN2-SD", k=3)
    classifier3 = learner2(data)
    classifier3.printAll()

    learner4 = SD_learner(algorithm = "SD-Preprocess", minSupport = 0.1, beamWidth = 5, g = 1)
    classifier4 = learner4(data)
    classifier4.printAll()

    print "___________________________"
    for d in data:
        print d.getclass(), classifier2(d, orange.GetValue), classifier3(d, orange.GetValue), classifier4(d, orange.GetValue)


    import cPickle
    one= classifier2.rulesClass[0].rules.rules[0]
    
    for obj in dir(one):
        try:
            cPickle.dump(getattr(one, obj), open('foo.pkl','w'))
            print obj, 'ok'
        except Exception, e:
            print obj, str(e)


    print "\n\n---> PMML model <---"
    result = classifier2.toPMML();
    if isinstance(result, file):
        print 'Result in file ', result.name
    elif isinstance(result, cStringIO.OutputType):
        print result.getvalue()
    else:
        raise TypeError
    result.close()


