
# naive bayes
# from sklearn import datasets
# iris = datasets.load_iris()
# from sklearn.naive_bayes import GaussianNB
# gnb = GaussianNB()
# y_pred = gnb.fit(iris.data, iris.target).predict(iris.data)
# print "Number of mislabeled points : %d" % (iris.target != y_pred).sum()

#print iris
#print iris.data
# print iris.target


# this code converts data from the csv file into scikit learn dataset and returns it as a tuple


def getDataFromCSV():
	from numpy import genfromtxt
	my_data = genfromtxt('iris.csv', delimiter=',')
	n_sample = []
	n_feature = []
	attributes = []
	for x in my_data:
		n_feature.append(x[-1])
		n_sample.append(x[:-1])
	dataset = (n_sample, n_feature, attributes)
	dictionary={"data":dataset}
	return dictionary


def naiveBayes(dictionary):
	dataset = dictionary["data"]
	n_sample = dataset[0]
	n_feature = dataset[1]	
	from sklearn.naive_bayes import GaussianNB
	gnb = GaussianNB()
	y_pred = gnb.fit(n_sample, n_feature).predict(n_sample)
	print "Expected Classification"
	print n_feature
	print "Predicted Classification"
	print y_pred
	print "Number of mislabeled points : %d" % (n_feature != y_pred).sum()

#data = getDataFromCSV()
#print data



# *********************			*************************		***********************



# GENERATE DECISION TREE PDF

# def decisionTreeJ48():
# 	from sklearn.datasets import load_iris
# 	from sklearn import tree
# 	iris = load_iris()
# 	clf = tree.DecisionTreeClassifier()
# 	clf = clf.fit(iris.data, iris.target)
# 	from StringIO import StringIO
# 	out = StringIO()
# 	out = tree.export_graphviz(clf, out_file=out)
# 	print out.getvalue()
# 	import StringIO, pydot 
# 	dot_data = StringIO.StringIO() 
# 	tree.export_graphviz(clf, out_file=dot_data) 
# 	graph = pydot.graph_from_dot_data(dot_data.getvalue()) 
# 	graph.write_pdf("iris.pdf")
# 	#graph_from_dot_data(out.getvalue()).write_pdf("iris.pdf")

# decisionTreeJ48()

# GENERATE DECISION TREE PNG

def decisionTreeJ48(dictionary):
	dataset = dictionary["data"]
	n_sample = dataset[0]
	n_feature = dataset[1]	
	from sklearn import tree
	clf = tree.DecisionTreeClassifier()
	clf = clf.fit(n_sample, n_feature)
	from StringIO import StringIO
	out = StringIO()
	out = tree.export_graphviz(clf, out_file=out)
	import StringIO, pydot 
	from os import system
	dot_data = StringIO.StringIO() 
	dotfile = open("decisionTreeJ48-scikit.dot", 'w')
	dotfile = tree.export_graphviz(clf, out_file=dotfile) 
	dotfile.close()
	system("dot -Tpng decisionTreeJ48-scikit.dot -o decisionTreeJ48-scikit.png")

# data = getDataFromCSV()
# decisionTreeJ48(data)


def ScikitDatasetToCSV(dictionary):
	dataset = dictionary["data"]
	n_sample = dataset[0]
	n_feature = dataset[1]
	import numpy
	csv=[]
	count=0
	for sample in n_sample:
	 	csv.append(numpy.append(sample,n_feature[count])) #join n_sample and n_feature array
	 	count+=1
	numpy.savetxt("foo.csv", csv, delimiter=",")

#data = getDataFromCSV()


# def ScikitDatasetToTAB(dictionary):
# 	dataset = dictionary["data"]
# 	n_sample = dataset[0]
# 	n_feature = dataset[1]
# 	import numpy
# 	csv=[]
# 	count=0
# 	for sample in n_sample:
# 	 	csv.append(numpy.append(sample,n_feature[count])) #join n_sample and n_feature array
# 	 	count+=1
# 	numpy.savetxt("foo.csv", csv, delimiter="	")
# 	my_data = genfromtxt("foo.csv", delimiter="	")
# 	print my_data 

# ScikitDatasetToTAB(data)

def NNK(input_dict):
	dataset = input_dict["data"]
	n_sample = dataset[0]
	n_feature = dataset[1]
	from sklearn.neighbors import KNeighborsClassifier
	knn = KNeighborsClassifier().fit(n_sample,n_feature)
	print knn




   
def SVC(input_dict):
	dataset = input_dict["data"]
	n_sample = dataset[0]
	n_feature = dataset[1]
	from sklearn.svm import SVC
	clf = SVC().fit(n_sample,n_feature)
	print clf
# SVC(data)






def nearestNeighbour():
	import numpy as np
	import pylab as pl
	from matplotlib.colors import ListedColormap
	from sklearn import datasets
	from sklearn.neighbors import NearestCentroid

	n_neighbors = 15

	# import some data to play with
	iris = datasets.load_iris()
	X = iris.data[:, :2]  # we only take the first two features. We could
	                      # avoid this ugly slicing by using a two-dim dataset
	y = iris.target

	h = .02  # step size in the mesh

	# Create color maps
	cmap_light = ListedColormap(['#FFAAAA', '#AAFFAA', '#AAAAFF'])
	cmap_bold = ListedColormap(['#FF0000', '#00FF00', '#0000FF'])

	for shrinkage in [None, 0.1]:
	    # we create an instance of Neighbours Classifier and fit the data.
	    clf = NearestCentroid(shrink_threshold=shrinkage)
	    clf.fit(X, y)
	    y_pred = clf.predict(X)
	    print shrinkage, np.mean(y == y_pred)
	    # Plot the decision boundary. For that, we will asign a color to each
	    # point in the mesh [x_min, m_max]x[y_min, y_max].
	    x_min, x_max = X[:, 0].min() - 1, X[:, 0].max() + 1
	    y_min, y_max = X[:, 1].min() - 1, X[:, 1].max() + 1
	    xx, yy = np.meshgrid(np.arange(x_min, x_max, h),
	                         np.arange(y_min, y_max, h))
	    Z = clf.predict(np.c_[xx.ravel(), yy.ravel()])

	    # Put the result into a color plot
	    Z = Z.reshape(xx.shape)
	    pl.figure()
	    pl.pcolormesh(xx, yy, Z, cmap=cmap_light)

	    # Plot also the training points
	    pl.scatter(X[:, 0], X[:, 1], c=y, cmap=cmap_bold)
	    pl.title("3-Class classification (shrink_threshold=%r)"
	             % shrinkage)
	    pl.axis('tight')

	# pl.show()

#nearestNeighbour()

#
#	REGRESSION EXAMPLES
#
#L2-regularized least squares linear model
def RidgeRegression(input_dict):
	# 	from sklearn.datasets import load_iris
# 	from sklearn import tree
# 	iris = load_iris()
# 	clf = tree.DecisionTreeClassifier()
# 	clf = clf.fit(iris.data, iris.target)	
	from sklearn.datasets import  load_diabetes
	diabetes = load_diabetes()
	n_sample = diabetes.data
	n_feature = diabetes.target
	print "*******SAMPLES********"
	print n_sample
	print "******FEARTURES*******"
	print n_feature
	from sklearn.linear_model import Ridge
	rgs = Ridge().fit(n_sample, n_feature)
	print rgs
	print rgs.predict(n_sample)



# L1+L2-regularized least squares linear model trained using Coordinate Descent
def ElasticNetRegression(input_dict):
	# 	from sklearn.datasets import load_iris
# 	from sklearn import tree
# 	iris = load_iris()
# 	clf = tree.DecisionTreeClassifier()
# 	clf = clf.fit(iris.data, iris.target)	
	from sklearn.datasets import  load_diabetes
	dta = load_diabetes()
	n_sample = dta.data
	n_feature = dta.target
	print "*******SAMPLES********"
	print n_sample
	print "******FEARTURES*******"
	print n_feature
	from sklearn.linear_model import ElasticNet
	rgs = ElasticNet().fit(n_sample, n_feature)
	print rgs
	print rgs.predict(n_sample)


# ElasticNetRegression(data)

def ClusteringKMeans():
	from sklearn import cluster, datasets
	iris = datasets.load_iris()
	X_iris = iris.data
	y_iris = iris.target
	k_means = cluster.KMeans(k=3) #number of clusters
	k_means.fit(X_iris) 
	print k_means.labels_
	print y_iris

#ClusteringKMeans()

def ClusteringMS():
	from sklearn import cluster, datasets
	iris = datasets.load_iris()
	X_iris = iris.data
	y_iris = iris.target
	k_means = cluster.SpectralClustering(3)
	k_means.fit(X_iris) 
	print k_means.labels_
	print y_iris

#ClusteringMS()


def test():
	print(__doc__)

	from sklearn import datasets, neighbors, linear_model

	digits = datasets.load_digits()
	X_digits = digits.data
	y_digits = digits.target

	n_samples = len(X_digits)

	X_train = X_digits[:.9 * n_samples]
	y_train = y_digits[:.9 * n_samples]
	X_test = X_digits[.9 * n_samples:]
	y_test = y_digits[.9 * n_samples:]

	knn = neighbors.KNeighborsClassifier()
	logistic = linear_model.LogisticRegression()

	print('KNN score: %f' % knn.fit(X_train, y_train).score(X_test, y_test))
	print('LogisticRegression score: %f'
	      % logistic.fit(X_train, y_train).score(X_test, y_test))

#test()


def scikitAlgorithms_UCIDataset(var):
    from sklearn import datasets
    allDSets = {"iris":datasets.load_iris(), "boston":datasets.load_diabetes(), "diabetes":datasets.load_boston()}
    dataset = allDSets[var]
    output_dict = dataset
    return output_dict

#iris = scikitAlgorithms_UCIDataset("iris")
#print iris




def retrunRightType(value):
	try:
		a= float(value) if '.' in value else int(value) #return int or float
	except ValueError:
		a= value #return string
	print "input par", a
	print "is type of", type(a)

retrunRightType("ahas")
#print "input par", a
#print "is type of", type(a)



