import orange
from orange import EnumVariable
import math

class Feature(orange.EnumVariable):
    def __init__(self):
        orange.EnumVariable.__init__(self, values = ["False", "True"])
        self.vals = []
        self.TP = set()
        self.TN = set()
    def set_parms(self, attribute, cond, value):
        self.name = attribute.name + cond + str(value)
        self.attribute = attribute
        self.cond = cond
        self.value = value

def generateFeatures(data, targetClass):
    
    if data.hasMissingClasses():
        print "Error: data has missing class values."
        return
    if not (data.domain.hasContinuousAttributes() or data.domain.hasDiscreteAttributes()):
        print "Error: data has to have at least one discrete or continuous attribute"
        return
    
    features = []
    for attribute in data.domain.attributes:
        if attribute.varType == orange.VarTypes.Discrete:
            
            # Find values for positive (v) and negative (w) examples.
            v, w = [],[]
            for example in data:
                if example[data.domain.classVar] == targetClass:
                    v.append(example[attribute].value)
                else:
                    w.append(example[attribute].value)
            # Delete duplicates
            v = list(set(v))
            w = list(set(w))
            
            for value in v:
                feature = Feature()
                feature.set_parms(attribute, "==", value)
                features.append(feature)
                for example in data:
                    # Handle undefined values (i type of value is "don't know", "don't care" or any other special type)
                    if (example[attribute].valueType != 0):
                        # If the current example is positive, value of the feature is False, else it's True
                        feature.vals.append(example[data.domain.classVar].value != targetClass)
                    else:
                        feature.vals.append(example[attribute].value == value)
            for value in w:
                feature = Feature()
                feature.set_parms(attribute, "!=", value)
                features.append(feature)
                for example in data:
                    # Handle undefined values (i type of value is "don't know", "don't care" or any other special type)
                    if (example[attribute].valueType != 0):
                        # If the current example is positive, value of the feature is False, else it's True
                        feature.vals.append(example[data.domain.classVar].value != targetClass)
                    else:
                        feature.vals.append(example[attribute].value != value)


        elif attribute.varType == orange.VarTypes.Continuous:
            # Sort by the current attribute
            data.sort(attribute, data.domain.classVar)
            # Go through all the values of current attribute and add a feature for each
            # neighbouring pair (ep,en) or (en,ep), where 'ep' is a positive na 'en' a negative example.
            for i in xrange(len(data) - 1):
                neighbors = (data[i][data.domain.classVar].value, data[i+1][data.domain.classVar].value)
                if data[i][attribute].valueType != 0 or data[i+1][attribute].valueType != 0:
                    continue
                
                if neighbors[0] != neighbors[1]:
                    if neighbors[0] == targetClass:
                        value = (data[i][attribute].value + data[i+1][attribute].value) / 2
                        # add feature attribute
                        feature = Feature()
                        feature.set_parms(attribute, "<=", value)
                        features.append(feature)
                        for example in data:
                            # Handle undefined values (if type of value is "don't know", "don't care" or any other special type)
                            if (example[attribute].valueType != 0):
                                # If the current example is positive, value of the feature is False, else it's True
                                feature.vals.append(example[data.domain.classVar].value != targetClass)
                            else:
                                feature.vals.append(example[attribute].value <= value)
                    elif neighbors[1] == targetClass:
                        value = (data[i][attribute].value + data[i+1][attribute].value) / 2
                        # add feature attribute
                        feature = Feature()
                        feature.set_parms(attribute, ">", value)
                        features.append(feature)
                        for example in data:
                            # Handle undefined values (if type of value is "don't know", "don't care" or any other special type)
                            if (example[attribute].valueType != 0):
                                # If the current example is positive, value of the feature is False, else it's True
                                feature.vals.append(example[data.domain.classVar].value != targetClass)
                            else:
                                feature.vals.append(example[attribute].value > value)
        
            # === !!! Integer value attributes -maybe later !!! ===
    
    # Class variable
#    classVar = Feature(data.domain.classVar)
    classValues = [example[data.domain.classVar].value for example in data]
    
    # Get TPs and TNs for each feature
    for feature in features:
        for i, value in enumerate(feature.vals):
            # Positive
            if classValues[i] == targetClass:
                if value:
                    feature.TP.add(i)
            # Negative
            elif not value:
                    feature.TN.add(i)
    
    # Number of positive examples
    noP = classValues.count(targetClass)
    
    minTP = noP / 2
    minTN = math.sqrt(len(data) - noP)
    
    # Absolute and total relevancy
    f = 0
    while f < len(features):
        if len(features[f].TP) <= minTP or len(features[f].TN) <= minTN:
            del features[f]
            continue
        f += 1
    
    # Relative relevancy
    f = 0
    while f < len(features) - 1:
        g = f + 1
        while g < len(features):
            # If f is relatively irrelevant, remove it
            if features[f].TP.issubset(features[g].TP) and features[f].TN.issubset(features[g].TN):
                del features[f]
                break
            # If g is relatively irrelevant, remove it
            elif features[f].TP.issuperset(features[g].TP) and features[f].TN.issuperset(features[g].TN):
                del features[f]
                continue
            g += 1
        else:
            # Only executed when while loops end normally - f was not removed
            f += 1
    
    domain = orange.Domain(features + [data.domain.classVar])
#    examples = [[feature.values[i] for feature in features] + [classValues[i]] for i in xrange(len(data))]
    examples = [[feature.vals[i] for feature in features] + [classValues[i]] for i in xrange(len(data))]
    
    
    return orange.ExampleTable(domain, examples)
    
