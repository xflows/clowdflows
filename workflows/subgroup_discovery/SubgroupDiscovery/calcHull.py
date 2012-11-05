def calcRates(subgroups):
        subgroups.TPR = []
        subgroups.FPR = []
        P = len(subgroups.targetClassRule.TP) * 1.0  # number of all positive examples as a float
        N = len(subgroups.targetClassRule.FP) * 1.0  # number of all negative examples as a float
        for rule in subgroups.rules:
            subgroups.TPR.append( len(rule.TP) / P )  # true positive rate for this rule
            subgroups.FPR.append( len(rule.FP) / N )  # false positive example for this rule

      #  subgroups.TPR = [0.44, 0.34,   0.33,   0.49,   0.43,   0.49,   0.66,   0.60,   0.61,   0.78,   0.75,   0.77,   0.84,   0.82,   0.82]
      #  subgroups.FPR = [0.01, 0.00,   0.00,   0.02,   0.00,   0.02,   0.10,   0.07,   0.07,   0.21,   0.16,   0.19,   0.31,   0.29,   0.27]

        len(subgroups.TPR)

        # calculate convex hull ,important: stick this 5 linet together
        subgroups.hullTPR = [0]
        subgroups.hullFPR = [0]
        calcHull(subgroups, subgroups.TPR[:], subgroups.FPR[:] , A=(0,0), B=(1,1))
        subgroups.hullTPR.append(1)
        subgroups.hullFPR.append(1)

def calcRatesSubset(subgroups):
    subgroups.TPR = []
    subgroups.FPR = []
    P = len(subgroups.targetClassRule.TP) * 1.0  # number of all positive examples as a float
    N = len(subgroups.targetClassRule.FP) * 1.0  # number of all negative examples as a float
    for rule in subgroups.rules:
        TPr = len(rule.TP) / P
        FPr = len(rule.FP) / N
        subgroups.TPR.append( TPr )  # true positive rate for this rule
        subgroups.FPR.append( FPr )  # false positive example for this rule
        #self.graphROC.tooltipData(FPr, TPr, rule)

def calcHull(subgroups, Y, X, A, B):
        #inicialization
        C = (-1,-1)    # best new point point
        y = -1         # best distance
        index = -1
        # calculate best new point
        if (B[0]-A[0])==0:
            #self.edtRules.appendPlainText("vertical line!!!")
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

            calcHull(subgroups, Yl, Xl, A,C)  # recursive call
            # save the new point
            subgroups.hullTPR.append(C[1])
            subgroups.hullFPR.append(C[0])
            # recursivey call this function on the RIGHT side of the point
            calcHull(subgroups, Yu, Xu, C,B)  # recursive call
