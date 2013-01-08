from __future__ import absolute_import

import numpy

import orange, statc

from . import stats

def mean(l):
    return float(sum(l))/len(l)

class MA_pearsonCorrelation:
    """
    Calling an object of this class computes Pearson correlation of all
    attributes against class.
    """
    def __call__(self, i, data):
        dom2 = orange.Domain([data.domain.attributes[i]], data.domain.classVar)
        data2 = orange.ExampleTable(dom2, data)
        a,c = data2.toNumpy("A/C")
        return numpy.corrcoef(c,a[:,0])[0,1]

class MA_signalToNoise:
    """
    Returns signal to noise measurement: difference of means of two classes
    divided by the sum of standard deviations for both classes. 

    Usege similar to MeasureAttribute*.

    Standard deviation used for now returns minmally 0.2*|mi|, where mi=0 is adjusted to mi=1
    (as in gsea implementation).

    Can work only on data with two classes. If there are multiple class, then
    relevant class values can be specified on object initialization.
    By default the relevant classes are first and second class value
    from the domain.
    """

    def __init__(self, a=None, b=None):
        """
        a and b are choosen class values.
        """
        self.a = a
        self.b = b

    def __call__(self, i, data):
        cv = data.domain.classVar
        #print data.domain

        if self.a == None: self.a = cv.values[0]
        if self.b == None: self.b = cv.values[1]

        def stdev(l):
            return statc.std(l)

        def mean(l):
            return statc.mean(l)

        def stdevm(l):
            m = mean(l)
            std = stdev(l)
            #return minmally 0.2*|mi|, where mi=0 is adjusted to mi=1
            return max(std, 0.2*abs(1.0 if m == 0 else m))

        def avWCVal(value):
            return [ex[i].value for ex in data if ex[-1].value == value and not ex[i].isSpecial() ]

        exa = avWCVal(self.a)
        exb = avWCVal(self.b)

        try:
            rval = (mean(exa)-mean(exb))/(stdevm(exa)+stdevm(exb))
            return rval
        except:
            #return some "middle" value -
            #TODO rather throw exception? 
            return 0

class MA_t_test(object):
    def __init__(self, a=None, b=None, prob=False):
        self.a = a
        self.b = b
        self.prob = prob
    def __call__(self, i, data):
        cv = data.domain.classVar
        #print data.domain

        #for faster computation. to save dragging many attributes along
        dom2 = orange.Domain([data.domain[i]], data.domain.classVar)
        data = orange.ExampleTable(dom2, data)
        i = 0

        if self.a == None: self.a = cv.values[0]
        if self.b == None: self.b = cv.values[1]

        def avWCVal(value):
            return [ex[i].value for ex in data if ex[cv] == value and not ex[i].isSpecial() ]

        exa = avWCVal(self.a)
        exb = avWCVal(self.b)

        try:
            t, prob = stats.lttest_ind(exa, exb)
            return prob if self.prob else t
        except:
            return 1.0 if self.prob else 0.0

class MA_fold_change(object):
    def __init__(self, a=None, b=None):
        self.a = a
        self.b = b
    def __call__(self, i, data):
        cv = data.domain.classVar
        #print data.domain

        #for faster computation. to save dragging many attributes along
        dom2 = orange.Domain([data.domain[i]], data.domain.classVar)
        data = orange.ExampleTable(dom2, data)
        i = 0

        if self.a == None: self.a = cv.values[0]
        if self.b == None: self.b = cv.values[1]

        def avWCVal(value):
            return [ex[i].value for ex in data if ex[cv] == value and not ex[i].isSpecial() ]

        exa = avWCVal(self.a)
        exb = avWCVal(self.b)

        try:
            return mean(exa)/mean(exb)
        except:
            return 1

class MA_anova(object):
    def __init__(self, prob=False):
        self.prob = prob
    def __call__(self, i, data):
        cv = data.domain.classVar
        #print data.domain

        #for faster computation. to save dragging many attributes along
        dom2 = orange.Domain([data.domain[i]], data.domain.classVar)
        data = orange.ExampleTable(dom2, data)
        i = 0

        def avWCVal(value):
            return [ex[i].value for ex in data if ex[cv] == value and not ex[i].isSpecial() ]

        data = [avWCVal(val) for val in cv.values]

        try:
            f, prob = stats.lF_oneway(*tuple(data))
            return prob if self.prob else f
        except:
            return 1.0 if self.prob else 0.0

import numpy as np
import numpy.ma as ma

class ExpressionSignificance_Test(object):
    def __new__(cls, data, useAttributeLabels, **kwargs):
        self = object.__new__(cls)
        if kwargs:
            self.__init__(data, useAttributeLabels)
            return self.__call__(**kwargs)
        else:
            return self
    
    def __init__(self, data, useAttributeLabels=False):
        self.data = data
        self.useAttributeLabels = useAttributeLabels
        self.attr_labels, self.data_classes = self._data_info(data)
        self.attributes = [attr for attr in self.data.domain.attributes if attr.varType in [orange.VarTypes.Continuous, orange.VarTypes.Discrete]]
        self.classes = np.array(self.attr_labels if useAttributeLabels else self.data_classes)
        self.keys = range(len(data)) if useAttributeLabels else self.attributes
        self.array, _, _ = data.toNumpyMA()
        if self.useAttributeLabels:
            self.array = ma.transpose(self.array)
#        self.dim = 1 if useAttributeLabels else 0  
        self.dim = 0
        
    def _data_info(self, data):
        return [set(attr.attributes.items()) for attr in data.domain.attributes], [ex.getclass() for ex in data] if data.domain.classVar else [None]*len(data)
        
    def test_indices(self, target, classes=None):
        classes = self.classes if classes is None else classes
        
        def target_set(target):
            if isinstance(target, tuple):
                return set([target])
            else:
                assert(isinstance(target, set))
                return target
            
        if self.useAttributeLabels:
            if isinstance(target, list):
                ind = [[i for i, cl in enumerate(self.classes) if target_set(t).intersection(cl)] for t in target]
            else:
                target = target_set(target)
                
                ind1 = [i for i, cl in enumerate(self.classes) if target.intersection(cl)]
                ind2 = [i for i, cl in enumerate(self.classes) if not target.intersection(cl)]
                ind = [ind1, ind2]
        else:
            if isinstance(target, list):
                ind = [ma.nonzero(self.classes == t)[0] for t in target]
            else:
                if isinstance(target, (basestring, orange.Variable)):
                    target = set([target])
                else:
                    assert(isinstance(target, set))
                target = list(target)
                ind1 = [i for i, cl in enumerate(self.classes) if cl in target]
                ind2 = [i for i, cl in enumerate(self.classes) if cl not in target]
                ind = [ind1, ind2]
                
        return ind
    
    def __call__(self, target):
        raise NotImplementedError()
    
    def null_distribution(self, num, *args, **kwargs):
        kwargs = dict(kwargs)
        advance = lambda: None
        if "advance" in kwargs:
            advance = kwargs["advance"]
            del kwargs["advance"]
        results = []
        originalClasses = self.classes.copy()
        for i in range(num):
            np.random.shuffle(self.classes)
            results.append(self.__call__(*args, **kwargs))
            advance()
        self.classes = originalClasses
        return results
    
class ExpressionSignificance_TTest(ExpressionSignificance_Test):
    def __call__(self, target):
        ind1, ind2 = self.test_indices(target)
        t, pval = attest_ind(self.array[ind1, :], self.array[ind2, :], dim=self.dim)
        return zip(self.keys,  zip(t, pval))
        
class ExpressionSignificance_FoldChange(ExpressionSignificance_Test):
    def __call__(self, target):
        ind1, ind2 = self.test_indices(target)
        a1, a2 = self.array[ind1, :], self.array[ind2, :]
        fold = ma.mean(a1, self.dim)/ma.mean(a2, self.dim)
        return zip(self.keys, fold)
    
class ExpressionSignificance_SignalToNoise(ExpressionSignificance_Test):
    def __call__(self, target):
        ind1, ind2 = self.test_indices(target)
        a1, a2 = self.array[ind1, :], self.array[ind2, :]
        stn = (ma.mean(a1, self.dim) - ma.mean(a2, self.dim)) / (ma.sqrt(ma.var(a1, self.dim)) + ma.sqrt(ma.var(a2, self.dim)))
        return zip(self.keys, stn)
    
class ExpressionSignificance_ANOVA(ExpressionSignificance_Test):
    def __call__(self, target=None):
        if target is not None:
            indices = self.test_indices(target)
        else:
            indices = []
        f, prob = aF_oneway(*[self.array[ind, :] for ind in indices], **dict(dim=0))
        return zip(self.keys, zip(f, prob))
        
class ExpressionSignificance_ChiSquare(ExpressionSignificance_Test):
    def __call__(self, target):
        array = equi_n_discretization(self.array.copy(), intervals=5, dim=0)
        ind1, ind2 = self.test_indices(target)
        a1, a2 = array[ind1, :], array[ind2, :]
        dist1, dist2  = [], []
        dist = ma.zeros((array.shape[1], 2, 5))
        for i in range(5):
            dist1.append(ma.sum(ma.ones(a1.shape) * (a1 == i), 0))
            dist2.append(ma.sum(ma.ones(a2.shape) * (a2 == i), 0))
            dist[:, 0, i] = dist1[-1]
            dist[:, 1, i] = dist2[-1] 
        return zip(self.keys, achisquare_indtest(np.array(dist), dim=1))
        
class ExpressionSignificance_Info(ExpressionSignificance_Test):
    def __call__(self, target):
        array = equi_n_discretization(self.array.copy(), intervals=5, dim=1)
        
        ind1, ind2 = self.test_indices(target)
        a1, a2 = array[ind1, :], array[ind2, :]
        dist1, dist2 = [], []
        dist = ma.zeros((array.shape[1], 2, 5))
        for i in range(5):
            dist1.append(ma.sum(ma.ones(a1.shape) * (a1 == i), 0))
            dist2.append(ma.sum(ma.ones(a2.shape) * (a2 == i), 0))
            dist[:, 0, i] = dist1[-1]
            dist[:, 1, i] = dist2[-1]
        classinfo = entropy(np.array([len(ind1), len(ind2)]))
        E = ma.sum(entropy(dist, dim=1) * ma.sum(dist, 1), 1) / ma.sum(ma.sum(dist, 1), 1)
        return zip(self.keys, classinfo - E)
    
class ExpressionSignificance_MannWhitneyu(ExpressionSignificance_Test):
    def __call__(self, target):
        ind1, ind2 = self.test_indices(target)
        a, b = self.array[ind1, :], self.array[ind2, :]
#        results = [amannwhitneyu(a[:, i],b[:, i]) for i in range(a.shape[1])]
        results = [statc.mannwhitneyu(list(a[:, i]),list(b[:, i])) for i in range(a.shape[1])]
        
        return zip(self.keys, results)

def attest_ind(a, b, dim=None):
    """ Return the t-test statistics on arrays a and b over the dim axis.
    Returns both the t statistic as well as the p-value
    """
#    dim = a.ndim - 1 if dim is None else dim
    x1, x2 = ma.mean(a, dim), ma.mean(b, dim)
    v1, v2 = ma.var(a, dim), ma.var(b, dim)
    n1, n2 = (a.shape[dim], b.shape[dim]) if dim is not None else (a.size, b.size)
    df = float(n1+n2-2)
    svar = ((n1-1)*v1+(n2-1)*v2) / df
    t = (x1-x2)/ma.sqrt(svar*(1.0/n1 + 1.0/n2))
    if t.ndim == 0:
        return (t, statc.betai(0.5*df,0.5,df/(df+t**2)) if t is not ma.masked and df/(df+t**2) <= 1.0 else ma.masked)
    else:
        prob = [statc.betai(0.5*df,0.5,df/(df+tsq)) if tsq is not ma.masked and df/(df+tsq) <= 1.0 else ma.masked  for tsq in t*t]
        return t, prob

def aF_oneway(*args, **kwargs):
    dim = kwargs.get("dim", None)
    arrays = args
    means = [ma.mean(a, dim) for a in arrays]
    vars = [ma.var(a, dim) for a in arrays]
    lens = [ma.sum(ma.array(ma.ones(a.shape), mask=ma.asarray(a).mask), dim) for a in arrays]
    alldata = ma.concatenate(arrays, dim if dim is not None else 0)
    bign =  ma.sum(ma.array(ma.ones(alldata.shape), mask=alldata.mask), dim)
    sstot = ma.sum(alldata ** 2, dim) - (ma.sum(alldata, dim) ** 2) / bign
    ssbn = ma.sum([(ma.sum(a, dim) ** 2) / L for a, L in zip(arrays, lens)], dim)
#    print ma.sum(alldata, dim) ** 2 / bign, ssbn
    ssbn -= ma.sum(alldata, dim) ** 2 / bign
    sswn = sstot - ssbn
    dfbn = dfnum = float(len(args) - 1.0)
    dfwn = bign - len(args) # + 1.0
    F = (ssbn / dfbn) / (sswn / dfwn)
    if F.ndim == 0 and dfwn.ndim == 0:
        return (F,statc.betai(0.5 * dfwn, 0.5 * dfnum, dfwn/float(dfwn+dfnum*F)) if F is not ma.masked and dfwn/float(dfwn+dfnum*F) <= 1.0 \
                and dfwn/float(dfwn+dfnum*F) >= 0.0 else ma.masked)
    else:
        prob = [statc.betai(0.5 * dfden, 0.5 * dfnum, dfden/float(dfden+dfnum*f)) if f is not ma.masked and dfden/float(dfden+dfnum*f) <= 1.0 \
            and dfden/float(dfden+dfnum*f) >= 0.0 else ma.masked for dfden, f in zip (dfwn, F)]
        return F, prob
    
def achisquare_indtest(observed, dim=None):
    if observed.ndim == 2:
        observed = ma.array([observed])
        if dim is not None:
            dim += 1 
    if dim is None:
        dim = observed.ndim - 2
    rowtotal = ma.sum(observed, dim + 1)
    coltotal = ma.sum(observed, dim)
    total = ma.sum(rowtotal, dim)
    ones = ma.array(ma.ones(observed.shape))
    expected = ones * rowtotal.reshape(rowtotal.shape[:dim] + (-1, 1))
    a = ones * coltotal[..., np.zeros(observed.shape[dim], dtype=int),:]
    expected = expected * (a) / total.reshape((-1, 1, 1))
    chisq = ma.sum(ma.sum((observed - expected) ** 2 / expected, dim + 1), dim)
    return chisq
    
def equi_n_discretization(array, intervals=5, dim=1):
    count = ma.sum(ma.array(ma.ones(array.shape, dtype=int), mask=array.mask), dim)
    cut = ma.zeros(len(count), dtype=int)
    sarray = ma.sort(array, dim)
    r = count % intervals
    pointsshape = list(array.shape)
    pointsshape[dim] = 1
    points = []
    for i in range(intervals):
        cutend = cut + count / intervals + numpy.ones(len(r)) * (r > i)
        if dim == 1:
            p = sarray[range(len(cutend)), numpy.array(cutend, dtype=int) -1]
        else:
            p = sarray[numpy.array(cutend, dtype=int) -1, range(len(cutend))]
        points.append(p.reshape(pointsshape))
        cut = cutend
    darray = ma.array(ma.zeros(array.shape) - 1, mask=array.mask)
    darray[ma.nonzero(array <= points[0])] = 0
    for i in range(0, intervals):
        darray[ma.nonzero((array > points[i]))] = i + 1 
    return darray

def entropy(array, dim=None):
    if dim is None:
        array = array.ravel()
        dim = 0
    n = ma.sum(array, dim)
    array = ma.log(array) * array
    sum = ma.sum(array, dim)
    return (ma.log(n) - sum / n) / ma.log(2.0)

"""\
MA - Plot
=========

Functions for normalization of expression arrays and ploting
MA - Plots

Example::
    ## Load data from GEO
    >>> data = orange.ExampleTable("GDS1210.tab")
    ## Split data by columns into normal and cancer subsets
    >>> cancer, normal = data_split(data, [("disease state", "cancer"), ("disease state", "normal")])
    ## Convert to numpy MaskedArrays
    >>> cancer, normal = cancer.toNumpyMA("A")[0], normal.toNumpyMA("A")[0]
    ## Merge by averaging
    >>> cancer = merge_replicates(cancer)
    >>> normal = merge_replicates(normal)
    ## Plot MA-plot
    >>> MA_plot(cancer, normal)
    
"""

from Orange.orng import orngMisc
from numpy import median
def lowess(x, y, f=2./3., iter=3, progressCallback=None):
    """ Lowess taken from Bio.Statistics.lowess, modified to compute pairwise 
    distances inplace.
     
    lowess(x, y, f=2./3., iter=3) -> yest

    Lowess smoother: Robust locally weighted regression.
    The lowess function fits a nonparametric regression curve to a scatterplot.
    The arrays x and y contain an equal number of elements; each pair
    (x[i], y[i]) defines a data point in the scatterplot. The function returns
    the estimated (smooth) values of y.

    The smoothing span is given by f. A larger value for f will result in a
    smoother curve. The number of robustifying iterations is given by iter. The
    function will run faster with a smaller number of iterations.

    x and y should be numpy float arrays of equal length.  The return value is
    also a numpy float array of that length.

    e.g.
    >>> import numpy
    >>> x = numpy.array([4,  4,  7,  7,  8,  9, 10, 10, 10, 11, 11, 12, 12, 12,
    ...                 12, 13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 16, 16,
    ...                 17, 17, 17, 18, 18, 18, 18, 19, 19, 19, 20, 20, 20, 20,
    ...                 20, 22, 23, 24, 24, 24, 24, 25], numpy.float)
    >>> y = numpy.array([2, 10,  4, 22, 16, 10, 18, 26, 34, 17, 28, 14, 20, 24,
    ...                 28, 26, 34, 34, 46, 26, 36, 60, 80, 20, 26, 54, 32, 40,
    ...                 28, 26, 34, 34, 46, 26, 36, 60, 80, 20, 26, 54, 32, 40,
    ...                 32, 40, 50, 42, 56, 76, 84, 36, 46, 68, 32, 48, 52, 56,
    ...                 64, 66, 54, 70, 92, 93, 120, 85], numpy.float)
    >>> result = lowess(x, y)
    >>> len(result)
    50
    >>> print "[%0.2f, ..., %0.2f]" % (result[0], result[-1])
    [4.85, ..., 84.98]
    """
    n = len(x)
    r = min(int(numpy.ceil(f*n)), n - 1)
    
#    h = [numpy.sort(numpy.abs(x-x[i]))[r] for i in range(n)]
#    h, xtmp = numpy.zeros_like(x), numpy.zeros_like(x)
#    for i in range(n):
#        xtmp = numpy.abs(x - x[i], xtmp)
#        h[i] = numpy.sort(xtmp)[r]
#    w = numpy.clip(numpy.abs(([x]-numpy.transpose([x]))/h),0.0,1.0)
    dist = [x] - numpy.transpose([x])
    dist = numpy.abs(dist, dist)
    dist.sort(axis=1)
    h = dist[:, r]
    del dist

    w = [x]-numpy.transpose([x])
    w /= h
    w = numpy.abs(w, w)
    w = numpy.clip(w, 0.0, 1.0, w)
#    w = 1-w*w*w
    w **= 3
    w *= -1
    w += 1
#    w = w*w*w
    w **= 3
    yest = numpy.zeros(n)
    delta = numpy.ones(n)
    milestones = orngMisc.progressBarMilestones(iter*n)
    for iteration in range(iter):
        for i in xrange(n):
            weights = delta * w[:,i]
            weights_mul_x = weights * x
            b1 = numpy.ma.dot(weights,y)
            b2 = numpy.ma.dot(weights_mul_x,y)
            A11 = sum(weights)
            A12 = sum(weights_mul_x)
            A21 = A12
            A22 = numpy.ma.dot(weights_mul_x,x)
            determinant = A11*A22 - A12*A21
            beta1 = (A22*b1-A12*b2) / determinant
            beta2 = (A11*b2-A21*b1) / determinant
            yest[i] = beta1 + beta2*x[i]
            if progressCallback and (iteration*n + i) in milestones:
                progressCallback((100. * iteration*n + i) /  (iter * n))
        residuals = y-yest
        s = median(abs(residuals))
        delta[:] = numpy.clip(residuals/(6*s),-1,1)
        delta[:] = 1-delta*delta
        delta[:] = delta*delta
    return yest



def lowess2(x, y, xest, f=2./3., iter=3, progressCallback=None):
    """Returns estimated values of y in data points xest (or None if estimation fails).
    Lowess smoother: Robust locally weighted regression.
    The lowess function fits a nonparametric regression curve to a scatterplot.
    The arrays x and y contain an equal number of elements; each pair
    (x[i], y[i]) defines a data point in the scatterplot. The function returns
    the estimated (smooth) values of y.

    The smoothing span is given by f. A larger value for f will result in a
    smoother curve. The number of robustifying iterations is given by iter. The
    function will run faster with a smaller number of iterations.
    
    Taken from Peter Juvan's numpyExtn.py, modified for numpy, computes pairwise
    distances inplace
    """
    x = numpy.asarray(x, 'f')
    y = numpy.asarray(y, 'f')
    xest = numpy.asarray(xest, 'f')
    n = len(x)
    nest = len(xest)
    r = min(int(numpy.ceil(f*n)),n-1) # radius: num. of points to take into LR
#    h = [numpy.sort(numpy.abs(x-x[i]))[r] for i in range(n)]    # distance of the r-th point from x[i]
    dist = [x] - numpy.transpose([x])
    dist = numpy.abs(dist, dist)
    dist.sort(axis=1)
    h = dist[:, r]
    del dist # to free memory
    w = [x] - numpy.transpose([x])
    w /= h
    w = numpy.abs(w, w)
    w = numpy.clip(w, 0.0, 1.0, w)
#    w = numpy.clip(numpy.abs(([x]-numpy.transpose([x]))/h),0.0,1.0)
    w **= 3
    w *= -1
    w += 1
#    w = 1 - w**3 #1-w*w*w
    w **= 3
#    w = w**3 #w*w*w
#    hest = [numpy.sort(numpy.abs(x-xest[i]))[r] for i in range(nest)]    # r-th min. distance from xest[i] to x
    dist = [x] - numpy.transpose([xest])
    dist = numpy.abs(dist, dist)
    dist.sort(axis=1)
    hest = dist[:, r]
    del dist # to free memory
#    west = numpy.clip(numpy.abs(([xest]-numpy.transpose([x]))/hest),0.0,1.0)  # shape: (len(x), len(xest)
    west = [xest]-numpy.transpose([x])
    west /= hest
    west = numpy.abs(west, west)
    west = numpy.clip(west, 0.0, 1.0, west)
#    west = 1 - west**3 #1-west*west*west
    west **= 3
    west *= -1
    west += 1
#    west = west**3 #west*west*west
    west **= 3
    yest = numpy.zeros(n,'f')
    yest2 = numpy.zeros(nest,'f')
    delta = numpy.ones(n,'f')
    iter_count = iter*(nest + n) if iter > 1 else nest
    milestones = orngMisc.progressBarMilestones(iter_count)
    curr_iter = 0
    for iteration in range(iter):
        # fit xest
        for i in range(nest):
            weights = delta * west[:,i]
            b = numpy.array([numpy.sum(weights*y), numpy.sum(weights*y*x)])
            A = numpy.array([[numpy.sum(weights), numpy.sum(weights*x)], [numpy.sum(weights*x), numpy.sum(weights*x*x)]])
            beta = numpy.linalg.solve(A, b)
            yest2[i] = beta[0] + beta[1]*xest[i]
            if progressCallback and curr_iter in milestones:
                progressCallback(100. * curr_iter / iter_count)
            curr_iter += 1
                
        # fit x (to calculate residuals and delta)
        if iter > 1:
            for i in range(n):
                weights = delta * w[:,i]
                b = numpy.array([numpy.sum(weights*y), numpy.sum(weights*y*x)])
                A = numpy.array([[numpy.sum(weights), numpy.sum(weights*x)], [numpy.sum(weights*x), numpy.sum(weights*x*x)]])
                beta = numpy.linalg.solve(A,b)
                yest[i] = beta[0] + beta[1]*x[i]
                if progressCallback and curr_iter in milestones:
                    progressCallback(100. * curr_iter / iter_count)
                curr_iter += 1
            residuals = y-yest
            s = numpy.median(numpy.abs(residuals))
            delta = numpy.clip(residuals/(6*s), -1, 1)
            delta = 1-delta*delta
            delta = delta*delta
    return yest2


def attr_group_indices(data, label_groups):
    """ Return a two or more lists of indices into `data.domain` based on `label_groups`
    
    Example::
        cancer_indices, no_cancer_indices = attr_group_indices(data, [("disease state", "cancer"), ("disease state", "normal")])
    """
    ret = []
    for key, val in label_groups:
        ind = [i for i, attr in enumerate(data.domain.attributes) if attr.attributes.get(key, None) == val]
        ret.append(ind)
    return ret


def example_group_indices(data, attr, values):
    """ Return lists of indices into `data` for each `values` item that matches
    the example value at `attr` attribute
    
    Example::
        cls_ind1, cls_ind2 = example_group_indices(data, data.domain.classVar, ["Class 1", "Class 2"])
    """
    ret = [[] for _ in values]
    values_id = dict([(str(value), i) for i, value in enumerate(values)])
    for i, ex in enumerate(data):
        id = values_id.get(str(ex[attr]), None)
        if id is not None:
            ret[id].append(i)
    return ret
    
    
def data_group_split(data, label_groups):
    """ Split an `data` example table into two or more based on
    contents of iterable `label_groups` containing (key, value)
    pairs matching the labels of data attributes.
    
    Example::
        cancer, no_cancer = data_group_split(data, [("disease state", "cancer"), ("disease state", "normal")])
    """
    ret = []
    group_indices = attr_group_indices(data, label_groups)
    for indices in group_indices:
        attrs = [data.domain[i] for i in indices]
        domain = orange.Domain(attrs, data.domain.classVar)
        domain.addmetas(data.domain.getmetas())
        ret.append(orange.ExampleTable(domain, data))
    return ret
    
    
def select_indices(data, key, value, axis=1):
    """ Return indices into `data` (ExampleTable) along specified `axis`
    where:
        - if axis == 0 match data[i][key] == value
        - if axis == 1 match data.domain[i].attributes[key] == value 
    
    Example::
        cancer_ind = select_indices(data, key="disease state", value="cancer"), axis=1)
        normal_ind = select_indices(data, key="disease state", value=["normal"], axis=1) # value can be a list to specify more then one value
        
    """
    values = value if isinstance(value, list) else [value]
    if axis == 0:
        groups = example_group_indices(data, key, values)
    else:
        groups = attr_group_indices(data, [(key, val) for val in values])
        
    return sorted(reduce(set.union, groups, set()))


def select_data(data, key, value, axis=1):
    """ Return `data` (ExampleTable) subset along specified `axis` where
    where:
        - if axis == 0 match data[i][key] == value
        - if axis == 1 match data.domain[i].attributes[key] == value 
        .. note:: This preserves all meta attributes of the domain
    Example::
        cancer = select_data(data, "disease state", "cancer", axis=1)
        normal = select_data(data, "disease state", ["normal"], axis=1) # value can be a list to specify more then one value
        
    """
    indices = select_indices(data, key, value, axis)
    if axis == 0:
        examples = [data[i] for i in indices]
        return orange.ExampleTable(data.domain, examples)
    else:
        attrs = [data.domain[i] for i in indices]
        domain = orange.Domain(attrs, False)
        domain.addmetas(data.domain.getmetas())
        return orange.ExampleTable(domain, data)
    
    
def split_data(data, groups, axis=1):
    """ Split data (ExampleTable) along specified axis, where elements of 
    `groups` match `key` and `value` arguments of the `select_data`
    function 
    
    Example::
        cancer, normal = split_data(data, [("disease state", "cancer"), ("disease state", ["normal"])], axis=1)
    """
    res = []
    for key, value in groups:
        res.append(select_data(data, key, value, axis))
    return res

    
def geometric_mean(array):
    """ Return a geometric mean computed on a 1d masked array
    """
    array = numpy.ma.asanyarray(array)
    return numpy.power(reduce(lambda a,b: a*b, array.filled(1.), 1.0), 1./len(array))


def harmonic_mean(array):
    """ Return a harmonic mean computed ona a 1d masked array
    """
    array = numpy.ma.asanyarray(array)
    return len(array) / numpy.ma.sum(1. / array)


def merge_replicates(replicates, axis=0, merge_function=numpy.ma.average):
    """ Merge `replicates` (numpy.array) along `axis` using `merge_function`
    """
    return numpy.ma.apply_along_axis(merge_function, axis, replicates)


def ratio_intensity(G, R):
    """ return the log2(R/G), log10(R*G) as a tuple
    """
    log2Ratio = numpy.ma.log(R/G) / numpy.log(2)
    log10Intensity = numpy.ma.log10(R*G)
    return log2Ratio, log10Intensity


def MA_center_average(G, R):
    """ return the G, R by centering the average log2 ratio
    """
    center_est = numpy.ma.average(numpy.ma.log(R/G) / numpy.log(2))
    G = G * numpy.exp2(center_est)
    return G, R.copy()


def MA_center_lowess(G, R, f=2./3., iter=1, progressCallback=None):
    """ return the G, R by centering the average log2 ratio locally
    depending on the intensity using lowess (locally weighted linear regression)
    """
#    from Bio.Statistics.lowess import lowess
    ratio, intensity = ratio_intensity(G, R)
    valid = - (ratio.mask & intensity.mask)
    valid_ind = numpy.ma.where(valid)
    center_est = lowess(intensity[valid], ratio[valid], f=f, iter=iter, progressCallback=progressCallback)
    Gc, R = G.copy(), R.copy()
    Gc[valid] *= numpy.exp2(center_est)
    Gc.mask, R.mask = -valid, -valid
    return Gc, R


def MA_center_lowess_fast(G, R, f=2./3., iter=1, resolution=100, progressCallback=None):
    """return the G, R by centering the average log2 ratio locally
    depending on the intensity using lowess (locally weighted linear regression),
    approximated only in a limited resolution.
    """
    
    ratio, intensity = ratio_intensity(G, R)
    valid = - (ratio.mask & intensity.mask)
    resoluiton = min(resolution, len(intensity[valid]))
    hist, edges = numpy.histogram(intensity[valid], len(intensity[valid])/resolution)
    
    progressCallback2 = (lambda val: progressCallback(val/2)) if progressCallback else None 
    centered = lowess2(intensity[valid], ratio[valid], edges, f, iter, progressCallback=progressCallback2)

    progressCallback2 = (lambda val: progressCallback(50 + val/2)) if progressCallback else None
    centered = lowess2(edges, centered, intensity[valid], f, iter, progressCallback=progressCallback2)
    
    Gc, R = G.copy(), R.copy()
    Gc[valid] *= numpy.exp2(centered)
    Gc.mask, R.mask = -valid, -valid
    return Gc, R


def MA_plot(G, R, format="b."):
    """ Plot G, R on a MA-plot using matplotlib
    """
    import matplotlib.pyplot as plt
    ratio, intensity = ratio_intensity(G, R)
    plt.plot(intensity, ratio, format)
    plt.ylabel('M = log2(R/G')
    plt.xlabel('A = log10(R*G)')


def normalize_expression_data(data, groups, axis=1, merge_function=numpy.ma.average, center_function=MA_center_lowess_fast):
    """ A helper function that normalizes expression array example table, by centering the MA plot.
    
    """
    if isinstance(data, orange.ExampleTable):
        label_groups = [select_indices(data, key, value, axis) for key, value in groups]
        array, _, _ = data.toNumpyMA()
    
    merged = []
    for indices in label_groups:
        replicates = numpy.take(array, indices, axis=1)
        merged.append(merge_replicates(replicates, axis=1, merge_function=merge_function))
        
    ind1, ind2 = label_groups
    G, R = merged
    Gc, Rc = center_function(G, R)
    
    domain = orange.Domain(data.domain.attributes, data.domain.classVar)
    domain.addmetas(data.domain.getmetas())
    data = orange.ExampleTable(domain, data)
    
    GFactors = Gc/G
    
    if axis == 0:
        for i, gf in zip(ind1, GFactors):
            for attr in range(len(data[i])):
                if not data[i][attr].isSpecial():
                    data[i][attr] = float(data[i][attr]) * gf
    else:   
        for ex, gf in zip(data, GFactors):
            for i in ind1:
                if not ex[i].isSpecial():
                    ex[i] = float(ex[i]) * gf
    return data
    
    
def MA_zscore(G, R, window=1./5., padded=False, progressCallback=None):
    """ Return the Z-score of log2 fold ratio estimated from local
    distribution of log2 fold ratio values on the MA-plot
    """
    ratio, intensity = ratio_intensity(G, R)
    
    z_scores = numpy.ma.zeros(G.shape)
    sorted = list(numpy.ma.argsort(intensity))
    import math, random
    r = int(math.ceil(len(sorted)*window)) # number of window elements
    def local_indices(i, sorted):
        """ local indices in sorted (mirror padded if out of bounds)
        """
        start, end = i - r/2, i + r/2 + r%2
        pad_start , pad_end = [], []
        if start < 0:
            pad_start = sorted[:abs(start)]
            random.shuffle(pad_start)
            start = 0
        if end > len(sorted):
            pad_end = sorted[end - len(sorted):]
            random.shuffle(pad_end)
            end = len(sorted)
        
        if padded:
            return pad_start + sorted[start: end] + pad_end
        else:
            return sorted[start:end]
    
    milestones = orngMisc.progressBarMilestones(len(sorted))
    for i in range(len(sorted)):
        indices = local_indices(i, sorted)
        localRatio = numpy.take(ratio, indices)
        local_std = numpy.ma.std(localRatio)
        ind = sorted[i]
        z_scores[ind] = ratio[ind] / local_std
        if progressCallback and i in milestones:
            progressCallback(100. * i / len(sorted))
        
    z_scores._mask = - numpy.isfinite(z_scores)
    return z_scores
    
