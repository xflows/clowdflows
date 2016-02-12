import arff
import jpype as jp

import common


# ==================
# Classification
# ==================

def weka_random_tree(input_dict):
    '''A tree that considers K randomly chosen attributes at each node, and performs no pruning
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.trees.RandomTree')()
    model.setOptions(common.parse_options(input_dict['params']))

    sclassifier = common.serialize_weka_object(model)
    return {'classifier':sclassifier}

def weka_j48(input_dict):
    '''Weka decision tree learner J48
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.trees.J48')()
    model.setOptions(common.parse_options(input_dict['params']))

    sclassifier = common.serialize_weka_object(model)
    return {'classifier':sclassifier}


def weka_naive_bayes(input_dict):
    '''Naive Bayes classifier provided by Weka. Naive Bayes is a simple probabilistic classifier based on applying the Bayes' theorem.
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.bayes.NaiveBayes')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'classifier':sclassifier}



def weka_k_star(input_dict):
    '''Instance-Based learner K* by Weka
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.lazy.KStar')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'classifier':sclassifier}



def weka_rep_tree(input_dict):
    '''A REP Tree, which is a fast decision tree learner. Builds a decision/regression tree using information gain/variance and prunes it using reduced-error pruning
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.trees.REPTree')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'classifier':sclassifier}

def weka_random_forest(input_dict):
    '''Random Forest learner by Weka
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.trees.RandomForest')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'classifier':sclassifier}


def weka_multilayer_perceptron(input_dict):
    '''Feedforward artificial neural network, using backpropagation to classify instances
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.functions.MultilayerPerceptron')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'classifier':sclassifier}


def weka_smo(input_dict):
    '''A support vector classifier, trained using the Sequential Minimal Optimization (SMO) algorithm
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.functions.SMO')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'classifier':sclassifier}


def weka_logistic(input_dict):
    '''Logistic regression by Weka
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.functions.Logistic')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'classifier':sclassifier}


def weka_ibk(input_dict):
    '''K-nearest neighbours classifier by Weka
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.lazy.IBk')()
    model.setOptions(common.parse_options(input_dict['params']))

    sclassifier = common.serialize_weka_object(model)
    return {'classifier':sclassifier}


# ++++++++++++ rules ++++++++++++

def weka_jrip(input_dict):
    '''The RIPPER rule learner by Weka
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.rules.JRip')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'classifier':sclassifier}


# def weka_zeror(input_dict):
#     '''Weka's rulesZeroR classifier: predicts the mean (for a numeric class) or the mode (for a nominal class).
#     '''
#     if not jp.isThreadAttachedToJVM():
#         jp.attachThreadToJVM()
#
#     model = jp.JClass('weka.classifiers.rules.ZeroR')()
#     model.setOptions(common.parse_options(input_dict['params']))
#     sclassifier = common.serialize_weka_object(model)
#     return {'classifier':sclassifier}


def weka_libsvm(input_dict):
    i=1


# ==================
# Evaluation
# ==================

def weka_build_classifier(input_dict):
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    instances = common.deserialize_weka_object(input_dict['instances'])

    classifier = common.deserialize_weka_object(input_dict['learner'])

    if instances.classIndex() == -1:
        instances.setClassIndex(instances.numAttributes() - 1)
        # raise ValueError('Class not set!')

    classifier.buildClassifier(instances)
    sclassifier = common.serialize_weka_object(classifier)

    return {'classifier':sclassifier}

def weka_apply_classifier(input_dict):
    i=1

def weka_apply_classifier_and_get_instances(input_dict):
    i=1

def weka_apply_mapped_classifier(input_dict):
    i=1
def weka_apply_mapped_classifier_get_instances(input_dict):
    i=1

def weka_cross_validate(input_dict):
    i=1

def weka_build_clusterer(input_dict):
    i=1
def weka_apply_clusterer(input_dict):
    i=1


# ==================
# Utilities
# ==================


def weka_confusion_matrix_computations(input_dict):
    summary = input_dict['summary']
    class_index = int(input_dict['classIndex'])
    summary_lines = summary.split('\n')[3:]
    summary_lines.pop()
    if class_index>-1:
        line = summary_lines[class_index]
        splitline = line.split()
        tp_rate = splitline[0]
        fp_rate = splitline[1]
        precision = splitline[2]
        recall = splitline[3]
        f = splitline[4]
        auc = splitline[5]
    else:
        avg_line = summary_lines.pop()
        splitline = avg_line.split()[2:]
        tp_rate = splitline[0]
        fp_rate = splitline[1]
        precision = splitline[2]
        recall = splitline[3]
        f = splitline[4]
        auc = splitline[5]
    output_dict = {}
    output_dict['precision']=precision
    output_dict['recall']=recall
    output_dict['auc']=auc
    output_dict['tp_rate']=tp_rate
    output_dict['fp_rate']=fp_rate
    output_dict['f']=f
    return output_dict


def weka_get_attr_list(input_dict):
    '''
    Returns attribute values for a single attribute from the dataset. Defaults to the last attribute.
    E.g., useful for calculating classification statistics.
    '''
    arff_file = input_dict['arff_file']
    attr_name = input_dict.get('attr_name', None)
    attr_list = []
    dataset = arff.loads(arff_file)
    attr_idx = -1
    if attr_name:
        attr_idx = map(lambda x: x[0], dataset['attributes'].index(attr_name))
    for row in dataset['data']:
        attr_list.append(row[attr_idx])
    return {'attr_list': attr_list}

def weka_arff_to_weka_instances(input_dict):
    '''
    Reads a dataset into a format suitable for WEKA methods
    '''

    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    tmp = common.TemporaryFile(suffix='.arff')
    tmp.writeString(input_dict['arff'])

    try:
        class_index = int(input_dict['class_index'])
    except:
        class_index = None

    source = jp.JClass('weka.core.converters.ConverterUtils$DataSource')(tmp.name)
    instances = source.getDataSet()

    if class_index is None:
        print 'Warning: class is set to the last attribute!'
        class_index = instances.numAttributes() - 1
    elif class_index == -1:
        class_index = instances.numAttributes() - 1

    instances.setClassIndex(class_index)

    return {'instances':common.serialize_weka_object(instances)}

def weka_instances_to_arff(input_dict):
    '''
    Reads a dataset into a format suitable for WEKA methods
    '''

    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    instances = common.deserialize_weka_object(input_dict['instances'])

    arff = instances.toString()

    return {'arff':arff}

def weka_print_model(input_dict):
    '''
    Prints a WEKA model
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    try:
        model = common.deserialize_weka_object(input_dict['model'])
        return {'model_as_string':model.toString()}
    except:
        raise Exception("Only WEKA classifiers/models supported. Please provide a valid WEKA model.")


def weka_print_tree(input_dict):
    i=1


