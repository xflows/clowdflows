import re

# def scikitAlgorithms_create_integers(input_dict):
#     intStr = input_dict['intStr']
#     intList = []
#     for i in re.findall(r'\w+', intStr):
#         try:
#             intList.append(int(i))
#         except:
#             pass
#     if input_dict['sort'].lower() == "true":
#         intList.sort()
#     return {'intList':intList}

# def scikitAlgorithms_sum_integers(input_dict):
#     intList = input_dict['intList']
#     return {'sum':sum(intList)}

# def scikitAlgorithms_pre_filter_integers(input_dict):
#     return input_dict

# def scikitAlgorithms_post_filter_integers(postdata,input_dict,output_dict):
#     intListOut = postdata['intListOut']
#     intList = []
#     for i in intListOut:
#         try:
#             intList.append(int(i))
#         except:
#             pass
#     return {'intList': intList}

# def scikitAlgorithms_pre_display_summation(input_dict):
#     return {}

# def scikitAlgorithms_sumTwoIntegers(input_dict):
#     output_dict = {}
#     output_dict['sum'] = int(input_dict['int1']) + int(input_dict['int2'])
#     return  output_dict

# def scikitAlgorithms_myFirstAction(input_dict):
#     output_dict={}
#     output_dict['out1'] = input_dict['inp1']
#     return output_dict


#
# CLASSIFICATION ALGORITHMS
#

def scikitAlgorithms_naiveBayes(input_dict):
    from sklearn.naive_bayes import GaussianNB 
    y_pred = GaussianNB()
    output_dict={}
    output_dict['bayesout'] = y_pred
    return output_dict

def scikitAlgorithms_J48(input_dict):
    from sklearn import tree
    clf = tree.DecisionTreeClassifier()
    output_dict={}
    output_dict['treeOut'] = clf
    return output_dict

def scikitAlgorithms_linearSVC(input_dict):
    from sklearn.svm import LinearSVC
    clf = LinearSVC()
    output_dict={}
    output_dict['SVCout'] = clf
    return output_dict

def scikitAlgorithms_SVC(input_dict):
    from sklearn.svm import SVC
    clf = SVC()
    output_dict={}
    output_dict['SVCout'] = clf
    return output_dict

def scikitAlgorithms_kNearestNeighbors(input_dict):
    from sklearn.neighbors import KNeighborsClassifier
    knn = KNeighborsClassifier()
    output_dict={}
    output_dict['KNNout'] = knn
    return output_dict

def scikitAlgorithms_logiscticRegression(input_dict):
    from sklearn.linear_model import LogisticRegression
    clf = LogisticRegression()
    output_dict={}
    output_dict['LRout'] = clf
    return output_dict

#
#   REGRESSION
#

def scikitAlgorithms_Ridge(input_dict):
    from sklearn.linear_model import Ridge
    clf = Ridge()
    output_dict={}
    output_dict['out'] = clf
    return output_dict

def scikitAlgorithms_ElasticNet(input_dict):
    from sklearn.linear_model import ElasticNet
    clf = ElasticNet()
    output_dict={}
    output_dict['out'] = clf
    return output_dict

def scikitAlgorithms_LassoLARS(input_dict):
    from sklearn.linear_model import LassoLARS
    clf = LassoLARS()
    output_dict={}
    output_dict['out'] = clf
    return output_dict

def scikitAlgorithms_SGDRegressor(input_dict):
    from sklearn.linear_model import SGDRegressor
    clf = SGDRegressor()
    output_dict={}
    output_dict['out'] = clf
    return output_dict

def scikitAlgorithms_ARDRegression(input_dict):
    from sklearn.linear_model import ARDRegression
    clf = ARDRegression()
    output_dict={}
    output_dict['out'] = clf
    return output_dict

def scikitAlgorithms_SVR(input_dict):
    from sklearn.linear_model import SVR 
    clf = SVR()
    output_dict={}
    output_dict['out'] = clf
    return output_dict

#
#   CLUSTERING
#

def scikitAlgorithms_KMeans(input_dict):
    from sklearn import cluster
    k_means = cluster.KMeans(input_dict['clustersNum']) #number of clusters
    output_dict={}
    output_dict['out'] = k_means
    return output_dict


#
#   UTILITIES
#

def scikitAlgorithms_UCIDataset(input_dict):
    from sklearn import datasets
    allDSets = {"iris":datasets.load_iris(), "boston":datasets.load_diabetes(), "diabetes":datasets.load_boston(), " linnerud":datasets.load_linnerud()}
    dataset = allDSets[input_dict['dsIn']]
    output_dict = {}
    output_dict['dtsOut'] = (dataset.data, dataset.target)
    return output_dict

def scikitAlgorithms_CSVtoNumpy(input_dict):
    # the targer value must be in the last colum of the CSV file
    output_dict={}
    # this code converts data from the csv file into scikit learn dataset and returns it as a tuple
    import numpy
    my_data = numpy.genfromtxt(input_dict['fileIn'], delimiter=',')
    n_sample = []
    n_feature = []
    for x in my_data:
        n_feature.append(x[-1]) 
        n_sample.append(x[:-1])
    print n_sample
    print n_feature
    dataset = (n_sample, n_feature)
    output_dict['scikitDataset'] =  dataset
    return output_dict # returns a touple consiting of n_samples x n_features numpy array X and an array of length n_samples containing the targets y


def scikitAlgorithms_SVMtoScikitDataset(input_dict):
    output_dict={}
    from sklearn.datasets import load_svmlight_file
    X_train, y_train = load_svmlight_file(input_dict['fileIn'])
    output_dict['datasetOut'] = (X_train, y_train)
    return output_dict # returns a touple consiting of n_samples x n_features numpy array X and an array of length n_samples containing the targets y

def scikitAlgorithms_buildClassifier(input_dict):
    learner = input_dict['learner']
    data = input_dict['instances']
    n_sample = data[0]
    n_feature = data[1]

    classifier = learner.fit(n_sample, n_feature) #.predict(n_sample)

    output_dict = {'classifier': classifier}
    return output_dict

def scikitAlgorithms_applyClassifier(input_dict):

    classifier = input_dict['classifier']
    data = input_dict['data']

    y_pred = (data[0], classifier.predict(data[0]))
    new_data = y_pred #"Number of mislabeled points : %d" % (data[0] != y_pred).sum()

    output_dict = {'classes':new_data}
    return output_dict

def scikitAlgorithms_scikitDatasetToCSV(input_dict):
    output_dict={}
    dataset= input_dict['scikitDataset']
    n_sample = dataset[0]
    n_feature = dataset[1]
    import numpy
    csv=[]
    count=0
    for sample in n_sample:
        csv.append(numpy.append(sample,n_feature[count])) #join n_sample and n_feature array
        count+=1
    #numpy.savetxt("foo.csv", csv, delimiter=",")
    output_dict['CSVout'] = csv
    return output_dict

def scikitAlgorithms_CSVtoOrange(input_dict):
    import orange
    output_dict = {}
    output_dict['dataset'] = orange.ExampleTable(input_dict['file'])
    return output_dict

def scikitAlgorithms_displayDS(input_dict):
    return {}


def scikitAlgorithms_displayDecisionTree(input_dict):
    from sklearn import tree
    from StringIO import StringIO
    out = StringIO()
    out = tree.export_graphviz(input_dict['classifier'], out_file=out)
    import StringIO, pydot 
    from os import system
    dot_data = StringIO.StringIO() 
    dotfile = open("decisionTreeJ48-scikit.dot", 'w')
    dotfile = tree.export_graphviz(input_dict['classifier'], out_file=dotfile) 
    dotfile.close()
    system("dot -Tpng decisionTreeJ48-scikit.dot -o decisionTreeJ48-scikit.png") #CORRECT SO THAT IMAGE IS GOING TO BE SAVED IN THE CORRECT DIRECTORY


#     dataset = input_dict["data"]
#     n_sample = dataset[0]
#     n_feature = dataset[1]
#     import numpy
#     orangeDataset=[]
#     count=0
#     for sample in n_sample:
#         orangeDataset.append(numpy.append(sample,n_feature[count])) #join n_sample and n_feature array
#         count+=1
#     #numpy.savetxt("foo.tab", csv, delimiter="   ")

# def scikitAlgorithms_scikitDatasetToOrange(input_dict):
#     import orange
#     output_dict = {}
#     output_dict['dataset'] = orange.ExampleTable(input_dict['file'])
#     return output_dict





















