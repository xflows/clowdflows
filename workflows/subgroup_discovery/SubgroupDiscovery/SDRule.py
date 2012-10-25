import orange
import targetClassLearner
import sys
from xmlMaker import *
import cStringIO

infinity = 1e10000

#class SDRule (orange.Rule):
class SDRule :
    def __init__(self, data, targetClass, conditions=[] ,g =1):
        self.g = g
        self.data = data
        self.targetClass = targetClass
        self.filter = orange.Filter_values(domain = data.domain, conditions=conditions , conjunction =1)
        self.learner = targetClassLearner.Learner(targetClass=targetClass , name = "TargetClassClassifierRules")
        self.filterAndStore()   # set examples, classifier, distribution; TP, calculate quality, complexity, support

    def filterAndStore(self):
        self.examples = self.filter(self.data)           # set examples
        self.classifier = self.learner(self.examples)        # set classifier
        distribution = [0.0]* len(self.data.domain.classVar.values)
        self.complexity = len(self.filter.conditions)    # set rule complexity
        if len(self.examples)>0:
            for d in self.examples:
                distribution[int(d.getclass())]+=1
            distribution = map (lambda d: d/len(self.examples), distribution)
            self.classDistribution = orange.Distribution(distribution)  # set distribution
            self.TP = filter(lambda e: e.getclass()==self.targetClass, self.examples)   # True positives
            self.FP = filter(lambda e: e.getclass()!=self.targetClass, self.examples)   # flase positives
            TPlen = len(self.TP) * 1.0
            self.quality = TPlen / (len(self.FP) + self.g)   # set rule quality: generalization quocient
            self.support = 1.0* len(self.examples)/len(self.data)        # set rule support
            self.confidence = TPlen/len(self.examples)
        else:
            self.classDistribution = distribution
            self.TP= []
            self.FP= []
            self.quality = 0        # set rule quality: generalization kvocient
            self.support = 0        # set rule support

    def covers(self, example):
        if len(self.filter([example]))==0:
            return False
        else:
            return True

    def __call__(self, example, result_type=orange.GetValue):
        return self.classifier(example, result_type)


    def cloneAndAddCondition(self, attribute, value):
        '''Returns a copy of this rule which condition part is extended by attribute = value'''
        cond = self.filter.conditions[:]
        cond.append(
            orange.ValueFilter_discrete(
                        position = self.data.domain.attributes.index(attribute),
                        values = [orange.Value(attribute, value)])
                     )
        return SDRule (self.data, self.targetClass, cond, self.g)

    def isIrrelevant(self, rule):
        '''Returns True if self is irrelevant compared to rule.'''
        def isSubset(subset, set):
            if len(subset) > len(set):
                return False
            else:
                index = 0;
                for e in subset:
                    while index<len(set) and e != set[index]:
                        index+=1
                    if index >= len(set):
                        return False
                return True

        if isSubset(self.TP, rule.TP) and isSubset(rule.FP, self.FP):
            return True
        else:
            return False

#________________________

    def ruleToString(self):
        ret = ""
        for i,c in enumerate(self.filter.conditions):
            if i > 0:
                ret += "   "
            ret += self.data.domain[c.position].name
           # ret += str([self.data.domain[c.position].values[int(v)] for v in c.values])
            if self.data.domain[c.position].varType == orange.VarTypes.Discrete:
                if len(c.values) == 1:
                    ret += " = " + str(self.data.domain[c.position].values[int(c.values[0])])
                else:
                    ret += " = ("
                    for value in c.values:
                        ret += str(self.data.domain[c.position].values[int(value)]) + ", "
                    ret = ret[:-2] + ")"
            elif self.data.domain[c.position].varType == orange.VarTypes.Continuous:
                if c.min == float(-infinity):
                    if not c.outside:    # <=
                        ret += " <= %.3f" % c.max
                    else:
                        ret += " > %.3f" % c.max
                else:
                    ret += " = (%.3f,%.3f]" % (c.min, c.max)
        ret = ret + "  -> " + self.data.domain.classVar.name + " = " + str(self.classifier.defaultVal)
        return ret

    def printRule(self):
        return "quality= %2.2f complexity=%2d covered=%2d numTP=%2d numFP=%2d  conf=%2d| %s\n" % (self.quality, self.complexity, len(self.examples), len(self.TP), len(self.FP), self.confidence ,self.ruleToString())

    def getFixed(self, original_data):
        cond = []
        for c in self.filter.conditions:
            feature = self.data.domain.attributes[c.position]
            position = original_data.domain.attributes.index(feature.attribute)

            if feature.cond == '==':
                cond.append(
                        orange.ValueFilter_discrete(
                                position = position,
                                values = [orange.Value(feature.attribute, feature.value)])
                            )
            elif feature.cond == '!=':
                cond.append(
                        orange.ValueFilter_discrete(
                                position = position,
                                values = [orange.Value(feature.attribute, value) for value in feature.attribute.values if value != feature.value])
                            )
            elif feature.cond == '<=':
                cond.append(
                        orange.ValueFilter_continuous(
                                position = position,
                                max = feature.value,
                                min = float(-infinity),
                                outside = False)
                            )
            elif feature.cond == '>':
                cond.append(
                        orange.ValueFilter_continuous(
                                position = position,
                                max = feature.value,
                                min = float(-infinity),
                                outside = True)
                            )

        return SDRule(original_data, self.targetClass, cond, self.g)

    def getUndiscretized(self, original_data):
        cond = []
        for c in self.filter.conditions:
            d_attribute = self.data.domain.attributes[c.position]
            if d_attribute in original_data.domain.attributes:
                c.position = original_data.domain.attributes.index(d_attribute)
                cond.append(c)
            else:
                position = original_data.domain.attributes.index(original_data.domain[d_attribute.name[2:]])

                points = d_attribute.getValueFrom.transformer.points
                value_idx = int(c.values[0])

                if value_idx == 0: # '<='
                    cond.append(
                            orange.ValueFilter_continuous(
                                    position = position,
                                    max = points[0],
                                    min = float(-infinity),
                                    outside = False)
                                )
                elif 0 < value_idx < len(points): # (x,y]
                    cond.append(
                            orange.ValueFilter_continuous(
                                    position = position,
                                    max = points[value_idx],
                                    min = points[value_idx-1],     # zaprti interval '[' namesto odprti '('
                                    outside = False)
                                )
                elif value_idx == len(points): # '>'
                    cond.append(
                            orange.ValueFilter_continuous(
                                    position = position,
                                    max = points[-1],
                                    min = float(-infinity),
                                    outside = True)
                                )


        return SDRule(original_data, self.targetClass, cond, self.g)

#________________________________________________________________________________________________________
#________________________________________________________________________________________________________
#________________________________________________________________________________________________________



class SDRules(object):
    def __init__(self, listOfRules, targetClassRule, algorithmName="SD"):
        self.rules = listOfRules
        self.targetClassRule = targetClassRule
        self.algorithmName = algorithmName

    def makeSelection(self, indexes):
        """Returns the rules that are at specified indexes."""
        rulesSubset = []
        for i in indexes:
            rulesSubset.append(self.rules[i])
        return SDRules(rulesSubset, self.targetClassRule)

    def sortByConf(self):
        self.rules.sort(lambda x, y: -cmp(x.confidence, y.confidence))

    def printRules(self):
        for rule in self.rules:
            rs = rule.printRule()
            print rs

    # The parameter must be a writable object type that supports at least write(string) function
    # e.g. file, StringIO.StringIO, or a custom object type with the write(string) function
    # The resulting object must be closed manually by calling close()
    #
    def toPMML(self, outputObjType=cStringIO.StringIO):
        ''' Ouutput the ruleset in a PMML RuleSet XML schema. '''
        output = outputObjType()
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
        dataDictionary.setAttribute("numberOfFields", "%d"%len(self.targetClassRule.data.domain.variables))
        for var in self.targetClassRule.data.domain.variables:
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
        ruleSetModel.setAttribute("algorithmName", self.algorithmName)  # spremeni v dinamicno

        miningSchema = myXML.insertNewNode(ruleSetModel, "MiningSchema")
        miningField = myXML.insertNewNode(miningSchema, "MiningField")
        miningField.setAttribute("name", self.targetClassRule.data.domain.classVar.name)
        miningField.setAttribute("usageType", "predicted")
        for attr in self.targetClassRule.data.domain.attributes:
            miningField = myXML.insertNewNode(miningSchema, "MiningField")
            miningField.setAttribute("name", attr.name)
            miningField.setAttribute("usageType", "active")

#RuleSet
        ruleSet = myXML.insertNewNode(ruleSetModel, "RuleSet")
        ruleSet.setAttribute("defaultScore", self.targetClassRule.targetClass.value)
        ruleSet.setAttribute("recordCount", "%d"%len(self.targetClassRule.data))
        ruleSet.setAttribute("nbCorrect", "%d"%len(self.targetClassRule.TP))
        ruleSet.setAttribute("defaultConfidence", "%2.2f"%self.targetClassRule.confidence)

        ruleSelectionMethod = myXML.insertNewNode(ruleSet, "RuleSelectionMethod")
        ruleSelectionMethod.setAttribute("criterion", "weightedSum")
        #rules
        for i in range(len(self.rules)):
            rule = self.rules[i]
            simpleRule = myXML.insertNewNode(ruleSet, "SimpleRule")
            simpleRule.setAttribute("id", "%d"%(i+1))
            simpleRule.setAttribute("score", rule.targetClass.value)
            simpleRule.setAttribute("recordCount", "%d"%len(rule.examples))
            simpleRule.setAttribute("nbCorrect", "%d"%len(rule.TP))
            simpleRule.setAttribute("confidence", "%2.2f"%rule.confidence)
            simpleRule.setAttribute("weight", "%2.2f"%rule.confidence)
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

#________________________________________________________________________________________________________
#________________________________________________________________________________________________________
#________________________________________________________________________________________________________

if __name__=="__main__":
    filename = "..\\..\\doc\\datasets\\lenses.tab"
    if 'linux' in sys.platform:
        filename= "/usr/doc/orange/datasets/lenses.tab"
    data = orange.ExampleTable(filename)



    targetClass= orange.Value(data.domain.classVar, "soft")
    rules = [SDRule( data=data, targetClass=targetClass, g = 20)]
    print

    attr = data.domain.attributes[0]
    val = attr.firstvalue().nextvalue()
    rules.append ( rules[0].cloneAndAddCondition(attr, val))

    attr = data.domain.attributes[1]
    val = attr.firstvalue()
    rules.append ( rules[0].cloneAndAddCondition(attr, val))

    attr = data.domain.attributes[2]
    val = attr.firstvalue().nextvalue()
    rules.append ( rules[0].cloneAndAddCondition(attr, val))

    attr = data.domain.attributes[1]
    val = attr.firstvalue()
    rules.append ( rules[1].cloneAndAddCondition(attr, val))


    print("_____all rules_____")
    targetClassRule = SDRule(data, targetClass, conditions=[], g =1)
    rs = SDRules(rules, targetClassRule)
    rs.printRules()


    strobj = rs.toPMML();
    strobj.getvalue()

    print("_____rules 1,2_____")
    subset = rs.makeSelection([1,2])
    subset.printRules()


    for i in range(len(rules)):
        print "_____rule_____",i
        rules[i].printRule()
        for e in rules[i].TP:
            print e

    print

    for i in range(len(rules)):
        for j in range(len(rules)):
            if rules[i].isIrrelevant(rules[j]):
                print "--- rule",i,"is irrelevant becouse of rule",j
            else:
                print "++++++ rule",i,"is relevant compared to",j

