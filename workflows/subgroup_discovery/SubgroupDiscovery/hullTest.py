def calcHull(subgroups, Y, X, A, B):
        #inicialization
        C = (-1,-1)    # best new point point         
        y = -1         # best distance
        # calculate best new point
        if (B[0]-A[0])==0:
            print "vertical line!!!"
        else:
            k = (B[1]-A[1]) / (B[0]-A[0])  # coefficient of the line between A and B
            for i in range(len(Y)):        # check every point
                yn = Y[i] -( k * ( X[i] - A[0] ) + A[1])   # vertical distance between point i and line AB
                if yn>0 and yn > y:        # if new distance is the greatest so far
                    C = (X[i], Y[i])       # the new point is the best so far
                    y = yn

        # if new point on the hull was found
        if C != (-1,-1):
            # save the new point

            # recursivey call this function on the LEFT side of the point
            Xl =[] 
            Yl =[]
            for i in range(len(X)):
                if X[i]>A[0] and X[i]<C[0]:
                    Xl.append(X[i])
                    Yl.append(Y[i])
            calcHull(subgroups, Yl, Xl, A,C)  # recursive call

            subgroups.hullTPR.append(C[1]) 
            subgroups.hullFPR.append(C[0])

            # recursivey call this function on the RIGHT side of the point
            Xu =[]
            Yu =[]
            for i in range(len(X)):
                if X[i]>C[0] and X[i]<B[0]:
                    Xu.append(X[i])
                    Yu.append(Y[i])
            calcHull(subgroups, Yu, Xu, C,B)  # recursive call


def calcAUC(X,Y):
    area = 0
    for i in range(len(X)-1):
        x = X[i+1]-X[i]
        y1 = Y[i]
        y2 = Y[i+1]
        trapez = x* (y1+y2)/2
        area = area + trapez
    return area


class krneki:
    def __init__(self):
        self.kgr =1
        

##X = [  0.34, 0.55, 0.34, 0.57, 0.79, 0.64, 0.84, 0.72, 0.06, 0.65, 0.86, 0.49, 0.35, 0.46, 0.23, 0.75, 0.32, 0.81, 0.54, 0.21, 0.57, 0.34, 0.17, 0.86, 0.48, 0.19, 0.64, 0.24, 0.18, 0.64, 0.73, 0.78, 0.54, 0.21, 0.77, 0.43, 0.67, 0.65, 0.1,  0.12, 0.15, 0.23, 0.45, 0.37, 0.34, 0.23, 0.27, 0.19, 0.36, 0.23, 0.23, 0.26, 0.32, 0.46, 0.19, 0.76, 0.43, 0.26, 0.36, 0.47, 0.59, 0.54, 0.32, 0.23, 0.54, 0.56, 0.34, 0.26, 0.57, 0.67, 0.39, 0.54, 0.34, 0.67, 0.27, 0.32]
##Y = [  0.1,  0.12, 0.15, 0.23, 0.45, 0.37, 0.34, 0.23, 0.27, 0.19, 0.36, 0.23, 0.23, 0.26, 0.32, 0.46, 0.19, 0.76, 0.43, 0.26, 0.36, 0.47, 0.59, 0.54, 0.32, 0.23, 0.54, 0.56, 0.34, 0.26, 0.57, 0.67, 0.39, 0.54, 0.34, 0.67, 0.27, 0.32, 0.24, 0.55, 0.34, 0.57, 0.79, 0.64, 0.84, 0.72, 0.06, 0.65, 0.86, 0.49, 0.35, 0.46, 0.23, 0.75, 0.32, 0.81, 0.54, 0.21, 0.57, 0.34, 0.17, 0.86, 0.48, 0.19, 0.64, 0.24, 0.18, 0.64, 0.73, 0.78, 0.54, 0.21, 0.77, 0.43, 0.67, 0.65]


#    1      2     3  |   4    5     6  |  7     8     9  |  10    11    12  | 13    14    15  
X = [0.00, 0.00, 0.05, 0.00, 0.05, 0.05, 0.03, 0.27, 0.12, 0.46, 0.37, 0.37, 0.80, 0.92, 0.46]
Y = [0.25, 0.41, 0.28, 0.41, 0.28, 0.28, 0.46 ,0.65, 0.45, 0.74, 0.79, 0.64, 0.96, 0.98, 0.74]

subgroups = krneki()

subgroups.hullTPR = [0]
subgroups.hullFPR = [0]

        # important
calcHull(subgroups, Y, X , A=(0,0), B=(1,1))
subgroups.hullTPR.append(1)
subgroups.hullFPR.append(1)
print "the hull"
for i in range(len(subgroups.hullFPR)):
    print subgroups.hullFPR[i], "    ",subgroups.hullTPR[i]

print calcAUC(subgroups.hullFPR, subgroups.hullTPR)    