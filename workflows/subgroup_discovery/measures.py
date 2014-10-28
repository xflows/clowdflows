#
# Functions for evaluating subgroups
#
from math import sqrt, log, exp
from math import log
import random
import orange
from SubgroupDiscovery.SDRule import SDRule


def evaluate(posEx, negEx, rules):
    measures = []
    for _, measure in DISCRETE_TARGET_MEASURES:
        measures.append(measure(posEx, negEx, rules))
    return measures


def get_covered(rule, instances=False):
    if isinstance(rule, SDRule):
        if instances:
            return rule.examples
        else:
            return len(rule.examples)
    return len(rule['covered'])


def get_positive_covered(rule, instances=False):
    if isinstance(rule, SDRule):
        if instances:
            return rule.TP
        else:
            return len(rule.TP)
    return len(rule['posCovered'])


def get_example_id(example):
    if isinstance(example, orange.Example):
        return example.id
    return example['id']

def cov(posEx, negEx, rules):
    N = float(posEx + negEx) # number of all examples
    RS = float(len(rules))   # ruleset size
    
    avg_cov = 1/RS * sum([get_covered(r)/N for r in rules])
    
    return avg_cov


def sup(posEx, negEx, rules):
    positives = set()
    for r in rules:
        positives.update(map(lambda x: get_example_id(x), get_positive_covered(r, instances=True)))
        
    return len(positives)/float(posEx)
    

    
def sig(posEx, negEx, rules): 
    
    N = float(posEx + negEx) # number of all examples
    RS = float(len(rules))   # ruleset size
    
    avg_sig = 0
    for r in rules:
        covered = float(get_covered(r))
        posCovered = float(get_positive_covered(r))
        TP = posCovered
        FP = covered - posCovered
        
        if FP == 0:
            FP = 1e-15  
        try:
            avg_sig = avg_sig + 2 * (TP * log(TP/(posEx * covered/N)) + FP * log(FP/(negEx * covered/N)))
        except:
            print r
            print TP, posEx, covered, N, FP, negEx
        
    avg_sig = 1/RS * avg_sig
    
    return avg_sig


def wracc(posEx, negEx, rules):

    N = float(posEx + negEx) # number of all examples
    RS = float(len(rules))   # ruleset size
    
    avg_wracc = 0
    for r in rules:
        covered = float(get_covered(r))
        posCovered = float(get_positive_covered(r))
        
        avg_wracc = avg_wracc + covered/N * (posCovered/covered - posEx/N)
        
    avg_wracc = 1/RS * avg_wracc
    
    return avg_wracc


def auc(posEx, negEx, rules):
    X_FPr = []
    Y_TPr = []
    
    # compute FPr and TPr for each rule
    for r in rules:
        
        TP = get_positive_covered(r)
        FP = get_covered(r) - TP
        
        fpr = FP/float(negEx)
        tpr = TP/float(posEx)        
        
        X_FPr.append(fpr)
        Y_TPr.append(tpr)        
    
    x, y = [0], [0]
    _calcHull(rules, y, x, Y_TPr[:], X_FPr[:], (0,0), (1,1))
    x.append(1)
    y.append(1)
    
    # Compute AUC
    return _calcAUC(x, y)


def _calcHull(subgroups, hullTPR, hullFPR, Y, X, A, B):
        #inicialization
        C = (-1,-1)    # best new point point         
        y = -1         # best distance
        index = -1
        # calculate best new point
        if (B[0]-A[0])==0:
            pass
        else:
            k = (B[1]-A[1]) / (B[0]-A[0])  # coefficient of the line between A and B
            for i in range(len(Y)):        # check every point
                yn = Y[i] -( k * ( X[i] - A[0] ) + A[1])   # vertical distance between point i and line AB
                if yn>0 and yn > y:        # if new distance is the greatest so far
                    C = (X[i], Y[i])       # the new point is the best so far
                    y = yn
                    index = i

        # if new point on the hull was found
        if C != (-1,-1):
            # recursivey call this function on the LEFT side of the point
            del X[index]
            del Y[index]
            Xl =[] 
            Yl =[]
            Xu =[]
            Yu =[]
            for i in range(len(X)):
                if X[i]>=A[0] and X[i]<=C[0] and Y[i]>A[1]:
                    Xl.append(X[i])
                    Yl.append(Y[i])
                elif X[i]>=C[0] and X[i]<=B[0] and Y[i]>C[1]:
                    Xu.append(X[i])
                    Yu.append(Y[i])
                    
            _calcHull(subgroups, hullTPR, hullFPR, Yl, Xl, A,C)  # recursive call
            # save the new point
            hullTPR.append(C[1]) 
            hullFPR.append(C[0])
            # recursivey call this function on the RIGHT side of the point
            _calcHull(subgroups, hullTPR, hullFPR, Yu, Xu, C,B)  # recursive call


def _calcAUC(X,Y):
    area = 0.0
    for i in range(len(X)-1):
        x = X[i+1]-X[i]
        y1 = Y[i]
        y2 = Y[i+1]
        trapez = x* (y1+y2)/2
        area = area + trapez
    return area


DISCRETE_TARGET_MEASURES = (
    ('RS coverage', cov),
    ('RS support', sup),
    ('RS significance', sig),
    ('RS WRAcc', wracc),
    ('RS AUC', auc)
)
