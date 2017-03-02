import arff
import jpype as jp

import common


def weka_local_random_tree(input_dict):
    '''A tree that considers K randomly chosen attributes at each node, and performs no pruning
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.trees.RandomTree')()
    model.setOptions(common.parse_options(input_dict['params']))

    sclassifier = common.serialize_weka_object(model)
    return {'RandomTree_learner': sclassifier}


def weka_local_j48(input_dict):
    '''Weka decision tree learner J48
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.trees.J48')()
    model.setOptions(common.parse_options(input_dict['params']))

    sclassifier = common.serialize_weka_object(model)
    return {'J48_learner': sclassifier}


def weka_local_naive_bayes(input_dict):
    '''Naive Bayes classifier provided by Weka. Naive Bayes is a simple probabilistic classifier based on applying the Bayes' theorem.
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.bayes.NaiveBayes')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'Naive_Bayes_learner': sclassifier}


def weka_local_k_star(input_dict):
    '''Instance-Based learner K* by Weka
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.lazy.KStar')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'KStar_learner': sclassifier}


def weka_local_rep_tree(input_dict):
    '''A REP Tree, which is a fast decision tree learner. Builds a decision/regression tree using information gain/variance and prunes it using reduced-error pruning
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.trees.REPTree')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'REPTree_learner': sclassifier}


def weka_local_random_forest(input_dict):
    '''Random Forest learner by Weka
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.trees.RandomForest')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'RandomForest_learner': sclassifier}


def weka_local_multilayer_perceptron(input_dict):
    '''Feedforward artificial neural network, using backpropagation to classify instances
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.functions.MultilayerPerceptron')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'Multilayer_Perceptron_learner': sclassifier}


def weka_local_smo(input_dict):
    '''A support vector classifier, trained using the Sequential Minimal Optimization (SMO) algorithm
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.functions.SMO')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'SMO_learner': sclassifier}


def weka_local_ibk(input_dict):
    '''K-nearest neighbours classifier by Weka
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.lazy.IBk')()
    model.setOptions(common.parse_options(input_dict['params']))

    sclassifier = common.serialize_weka_object(model)
    return {'IBk_learner': sclassifier}


def weka_local_libsvm(input_dict):
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()
    model = jp.JClass('weka.classifiers.functions.LibSVM')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'LibSVM_learner': sclassifier}

def weka_local_generic_learner(input_dict):
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()
    model = jp.JClass(input_dict['weka_class'])()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'Generic_Weka_learner': sclassifier}

# ++++++++++++ rules ++++++++++++

def weka_local_jrip(input_dict):
    '''The RIPPER rule learner by Weka
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.rules.JRip')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'JRip_learner': sclassifier}


def weka_local_zeror(input_dict):
    '''Weka's rulesZeroR classifier: predicts the mean (for a numeric class) or the mode (for a nominal class).
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    model = jp.JClass('weka.classifiers.rules.ZeroR')()
    model.setOptions(common.parse_options(input_dict['params']))
    sclassifier = common.serialize_weka_object(model)
    return {'classifier': sclassifier}


# ==================
# Utilities
# ==================


def weka_local_confusion_matrix_computations(input_dict):
    summary = input_dict['summary']
    class_index = int(input_dict['classIndex'])
    summary_lines = summary.split('\n')[3:]
    summary_lines.pop()
    if class_index > -1:
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
    output_dict['precision'] = precision
    output_dict['recall'] = recall
    output_dict['auc'] = auc
    output_dict['tp_rate'] = tp_rate
    output_dict['fp_rate'] = fp_rate
    output_dict['f'] = f
    return output_dict


def weka_local_get_attr_list(input_dict):
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


def weka_local_arff_to_weka_instances(input_dict):
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

    return {'instances': common.serialize_weka_object(instances)}


def weka_local_instances_to_arff(input_dict):
    '''
    Reads a dataset into a format suitable for WEKA methods
    '''

    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    instances = common.deserialize_weka_object(input_dict['instances'])

    arff = instances.toString()

    return {'arff': arff}


def weka_local_print_model(input_dict):
    '''
    Prints a WEKA model
    '''
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    try:
        model = common.deserialize_weka_object(input_dict['model'])
        return {'model_as_string': model.toString()}
    except:
        raise Exception("Only WEKA classifiers/models supported. Please provide a valid WEKA model.")

def weka_local_display_decision_tree(input_dict):
    return {}

# ==================
# Evaluation
# ==================

def weka_local_build_classifier(input_dict):
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    instances = common.deserialize_weka_object(input_dict['instances'])
    classifier = common.deserialize_weka_object(input_dict['learner'])

    if instances.classIndex() == -1:
        instances.setClassIndex(instances.numAttributes() - 1)
        # raise ValueError('Class not set!')

    classifier.buildClassifier(instances)
    sclassifier = common.serialize_weka_object(classifier)

    return {'classifier': sclassifier}


def weka_local_apply_classifier(input_dict):
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    # print("Instances: %s" % type(input_dict['instances']))
    instances = common.deserialize_weka_object(input_dict['instances'])

    if instances.classIndex() == -1:
        instances.setClassIndex(instances.numAttributes() - 1)  # last attribute is class

    classifier_serialized = input_dict['classifier']
    predictions = []
    try:
        classifier = common.deserialize_weka_object(classifier_serialized)
        for instance in instances:
            label_ind = int(classifier.classifyInstance(instance))
            label = instances.attribute(instances.numAttributes() - 1).value(label_ind)
            predictions.append(label)

        return {'classes': predictions}
    except:
        raise Exception("Classifier not built. Please use the Build Classifier widget first.")


def weka_local_apply_classifier_and_get_instances(input_dict):
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    # print("Instances: %s" % type(input_dict['instances']))
    instances = common.deserialize_weka_object(input_dict['instances'])

    if instances.classIndex() == -1:
        instances.setClassIndex(instances.numAttributes() - 1)  # last attribute is class

    classifier_serialized = input_dict['classifier']
    try:
        classifier = common.deserialize_weka_object(classifier_serialized)
        classAttribute = instances.classAttribute()
        for instance in instances:
            label_ind = int(classifier.classifyInstance(instance))
            instance.setClassValue(classAttribute.value(label_ind))

        return {'instances': common.serialize_weka_object(instances)}
    except:
        raise Exception("Classifier not built. Please use the Build Classifier widget first.")


def weka_local_apply_mapped_classifier(input_dict):
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    MAPPING_REPORT_START = 'Attribute mappings:'

    classifier = common.deserialize_weka_object(input_dict['classifier'])
    original_training_instances = common.deserialize_weka_object(input_dict['original_training_instances'])
    instances = common.deserialize_weka_object(input_dict['instances'])

    # serialize classifier with original instances to a file once again for the Mapped classifier
    tfile = common.TemporaryFile(flags='wb+')
    s = jp.JClass('weka.core.SerializationHelper')
    s.writeAll(tfile.name, [classifier, original_training_instances])

    # construct a MappedClassifier
    mappedClassifier = jp.JClass('weka.classifiers.misc.InputMappedClassifier')()
    mappedClassifier.setIgnoreCaseForNames(True)
    mappedClassifier.setTrim(True)
    #mappedClassifier.setSuppressMappingReport(True)
    #mc.setModelHeader(original_training_instances)
    mappedClassifier.setModelPath(tfile.name)

    # use the mapped classifier on new data
    classes = []
    classIndex = instances.classIndex()
    if classIndex == -1:
        raise ValueError('Class not set!')
    classAttribute = instances.classAttribute()
    for instance in instances:
        label = int(mappedClassifier.classifyInstance(instance))
        classes.append(classAttribute.value(label))

    report = mappedClassifier.toString()
    if MAPPING_REPORT_START in report:
        report = report[report.index(MAPPING_REPORT_START):]

    return {'mapping_report':report, 'classes':classes}


def weka_local_apply_mapped_classifier_get_instances(input_dict):
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    MAPPING_REPORT_START = 'Attribute mappings:'

    classifier = common.deserialize_weka_object(input_dict['classifier'])
    original_training_instances = common.deserialize_weka_object(input_dict['original_training_instances'])
    instances = common.deserialize_weka_object(input_dict['instances'])

    # serialize classifier with original instances to a file once again for the Mapped classifier
    tfile = common.TemporaryFile(flags='wb+')
    s = jp.JClass('weka.core.SerializationHelper')
    s.writeAll(tfile.name, [classifier, original_training_instances])

    # construct a MappedClassifier
    mappedClassifier = jp.JClass('weka.classifiers.misc.InputMappedClassifier')()
    mappedClassifier.setIgnoreCaseForNames(True)
    mappedClassifier.setTrim(True)
    #mappedClassifier.setSuppressMappingReport(True)
    #mc.setModelHeader(original_training_instances)
    mappedClassifier.setModelPath(tfile.name)

    # use the mapped classifier on new data
    classIndex = instances.classIndex()
    if classIndex == -1:
        raise ValueError('Class not set!')
    classAttribute = instances.classAttribute()
    for instance in instances:
        label = int(mappedClassifier.classifyInstance(instance))
        instance.setClassValue(classAttribute.value(label))

    report = mappedClassifier.toString()
    if MAPPING_REPORT_START in report:
        report = report[report.index(MAPPING_REPORT_START):]

    return {'mapping_report':report, 'instances':common.serialize_weka_object(instances)}


def weka_local_cross_validate(input_dict):
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()
    try:
        num_folds = int(input_dict['folds'])
    except:
        num_folds = 10

    try:
        class_index = int(input_dict['classIndex'])
    except:
        class_index = -1

    instances = common.deserialize_weka_object(input_dict['instances'])

    if instances.classIndex() == -1:
        instances.setClassIndex(instances.numAttributes() - 1)  # last attribute is class

    classifier_serialized = input_dict['learner']
    try:
        classifier = common.deserialize_weka_object(classifier_serialized)
        eval = jp.JClass('weka.classifiers.Evaluation')(instances)
        rand = jp.JClass('java.util.Random')(1)
        eval.crossValidateModel(classifier, instances, num_folds, rand, [])

        if class_index == -1:
            pre, rec, f, auc, tp_r, fp_r = (eval.weightedPrecision(),
                                            eval.weightedRecall(),
                                            eval.weightedFMeasure(),
                                            eval.weightedAreaUnderROC(),
                                            eval.weightedTruePositiveRate(),
                                            eval.weightedTrueNegativeRate())
        else:
            pre, rec, f, auc, tp_r, fp_r = (eval.precision(class_index),
                                            eval.recall(class_index),
                                            eval.fMeasure(class_index),
                                            eval.areaUnderROC(class_index),
                                            eval.truePositiveRate(class_index),
                                            eval.trueNegativeRate(class_index))

        # collect predictions and their probabilities
        classAttribute = instances.classAttribute()
        classifier.buildClassifier(instances)
        actual_classes = []
        predicted_classes = []
        predicted_classes_probs = []
        for instance in instances:
            actual = classAttribute.value(int(instance.classValue()))
            predicted = classAttribute.value(int(classifier.classifyInstance(instance)))
            probs = classifier.distributionForInstance(instance)
            actual_classes.append(actual)
            predicted_classes.append(predicted)
            predicted_classes_probs.append({classAttribute.value(i): p for i,p in enumerate(probs)})

        target = input_dict.get('target')
        if not target:
            target = classAttribute.value(0)
            print('Warning: observing the first class value {}'.format(target))

        # compute input for Viper
        mname = str(classifier.__getattribute__('class'))
        mname = mname[mname.find('weka'):-2]
        name = 'target class "{}": {}'.format(target, mname)
        apv = {'actual':[], 'predicted':[], 'name': name}
        for i, (actual, predicted) in enumerate(zip(actual_classes, predicted_classes)):
            if target == actual:
                apv['actual'].append(1)
            else:
                apv['actual'].append(0)
            if predicted_classes[i] == target:
                apv['predicted'].append(predicted_classes_probs[i][target])
            else:
                apv['predicted'].append(0)


        return {'confusion_matrix': eval.toMatrixString(),
                'accuracy': 100 * (1 - eval.errorRate()),
                'summary': eval.toSummaryString("=== Summary ===", True),
                'accuracy_by_class': eval.toClassDetailsString(),
                'precision': pre,
                'recall': rec,
                'f': f,
                'auc': auc,
                'tp_rate': tp_r,
                'fp_rate': fp_r,
                'apv': apv}
    except Exception, e:
        raise Exception("Error in weka_local_cross_validate() : "+str(e))
