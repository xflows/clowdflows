from os.path import normpath, join, dirname

import common
import library


def test1():
    fn = normpath(join(dirname(__file__), 'weka', 'data', 'iris.arff'))
    instances = library.weka_local_arff_to_weka_instances({'arff': open(fn).read()})
    instances = instances['instances']

    # create a learner
    j48_learner = library.weka_local_j48({'params': None})
    j48_learner = j48_learner['J48_learner']

    # build classifier
    model = library.weka_local_build_classifier({'learner': j48_learner, 'instances': instances})
    model = model['classifier']

    model_as_string = library.weka_local_print_model({'model': model})
    model_as_string = model_as_string['model_as_string']

    # print J48 tree
    print model_as_string


def test2():
    learners = [library.weka_local_ibk({'params': None})['IBk_learner'],
                library.weka_local_j48({'params': None})['J48_learner'],
                library.weka_local_jrip({'params': None})['JRip_learner'],
                library.weka_local_k_star({'params': None})['KStar_learner'],
                library.weka_local_libsvm({'params': None})['LibSVM_learner'],
                library.weka_local_multilayer_perceptron({'params': None})['Multilayer_Perceptron_learner'],
                library.weka_local_naive_bayes({'params': None})['Naive_Bayes_learner'],
                library.weka_local_rep_tree({'params': None})['REPTree_learner'],
                library.weka_local_random_forest({'params': None})['RandomForest_learner'],
                library.weka_local_random_tree({'params': None})['RandomTree_learner'],
                library.weka_local_smo({'params': None})['SMO_learner']
                ]

    fn = normpath(join(dirname(__file__), 'weka', 'data', 'iris.arff'))
    instances = library.weka_local_arff_to_weka_instances({'arff': open(fn).read()})
    instances = instances['instances']
    iris_instances = common.deserialize_weka_object(instances)

    class_attribute = iris_instances.classAttribute()

    for learner in learners:
        print("Learner: " + str(learner))
        model = common.deserialize_weka_object(learner)
        model.buildClassifier(iris_instances)
        print '\nalgorithm: %s' % repr(type(model))[::-1][2:repr(type(model))[::-1].index('.')][::-1]
        for instance in iris_instances:
            original = int(instance.classValue())
            new = int(model.classifyInstance(instance))
            if original != new:
                print 'misclassified training example: %s was predicted as: %s' % (
                    str(instance), class_attribute.value(new))
        print

def test3():
    learners = [library.weka_local_j48({'params': None})['J48_learner'],
                library.weka_local_k_star({'params': None})['KStar_learner']]

    fn = normpath(join(dirname(__file__), 'weka', 'data', 'iris.arff'))
    instances = library.weka_local_arff_to_weka_instances({'arff': open(fn).read()})
    instances = instances['instances']
    cl_inds = [-1,0,1,2]

    for learner in learners:
        print("\nLearner: " + str(learner))

        for cl_ind in cl_inds:
            cv_res = library.weka_local_cross_validate({'instances':instances,
                                                        'learner':learner,
                                                        'classIndex':cl_ind})
            print("   Class index: " + str(cl_ind))

            for k in cv_res.keys():
                if type(cv_res[k]) is float:
                    print "     %20s : %.4f" % (k , cv_res[k])

def test4():
    generic_learners = ["weka.classifiers.bayes.BayesNet", "weka.classifiers.trees.HoeffdingTree"]

    fn = normpath(join(dirname(__file__), 'weka', 'data', 'iris.arff'))
    instances = library.weka_local_arff_to_weka_instances({'arff': open(fn).read()})
    instances = instances['instances']
    iris_instances = common.deserialize_weka_object(instances)
    class_attribute = iris_instances.classAttribute()

    for generic_learner in generic_learners:
        learner = library.weka_local_generic_learner({'weka_class':generic_learner,'params':None})
        learner = learner['Generic_Weka_learner']

        model = common.deserialize_weka_object(learner)
        model.buildClassifier(iris_instances)

        print '\nalgorithm: %s' % repr(type(model))[::-1][2:repr(type(model))[::-1].index('.')][::-1]
        for instance in iris_instances:
            original = int(instance.classValue())
            new = int(model.classifyInstance(instance))
            if original != new:
                print 'misclassified training example: %s was predicted as: %s' % (
                    str(instance), class_attribute.value(new))



if __name__ == '__main__':
    test1()
    test2()
    test3()
    test4()
