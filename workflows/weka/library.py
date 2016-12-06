import arff
from services.webservice import WebService
from scipy.io import arff as scipy_arff
from cStringIO import StringIO
import numpy
from sklearn.decomposition import LatentDirichletAllocation
from sklearn.decomposition import TruncatedSVD


def weka_statistics(input_dict):
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


def weka_lda(input_dict):
    arff_file_train = StringIO(input_dict['train'])
    arff_file_test = StringIO(input_dict['test'])
    data_train, meta_train = scipy_arff.loadarff(arff_file_train)
    data_test, meta_test = scipy_arff.loadarff(arff_file_test)
    n_topics = int(input_dict['n_topics'])
    n_iter = int(input_dict['n_iter'])
    relation_name = input_dict['relation_name']
    random_state = int(input_dict['random_state'])
    keep_original_dimensions = input_dict['keep_original']
    model = LatentDirichletAllocation(n_topics=n_topics, max_iter=n_iter, random_state=random_state)
    #model = TruncatedSVD(n_components=n_topics, n_iter=n_iter, random_state=random_state)
    data_train, meta_train, data_test, meta_test = list(data_train), list(meta_train), list(data_test), list(meta_test) 
    dataTable = []
    yTable = []
    
    #add missing attributes from testset to trainset
    for instance in data_train:
        row = []
        for attribute in instance:
            row.append(attribute)
        row = row[:-1]
        for attribute_name in meta_test:
            if attribute_name not in meta_train:
                row.append(0.0)
        dataTable.append(row)
        yTable.append(instance[-1])

    splitIndex = len(dataTable)

    #add missing attributes from trainset to testset
    for instance in data_test:
        row = []
        for attribute in meta_train:
            try:
                idx = meta_test.index(attribute)
                row.append(instance[idx])
            except:
                row.append(0.0)
        row = row[:-1]
        for i, attribute_name in enumerate(meta_test):
            if attribute_name not in meta_train:
                row.append(instance[i])
        dataTable.append(row)
        yTable.append(instance[-1])

    
    dataset = numpy.array(dataTable)
    model= model.fit_transform(dataset)
    lda_list = model.tolist()
    attributes = []
    attributes_train = []
    attributes_test = []
    for i in range(n_topics):
        attributes.append(('topic_' + str(i), u'REAL'))
    if keep_original_dimensions:
        attributes_train = []
        attributes_test = []
        for attribute in meta_train[:-1]:
            attributes_train.append((attribute, u'REAL'))
        for attribute in meta_test[:-1]:
            attributes_test.append((attribute, u'REAL'))
        for i, row in enumerate(lda_list[:splitIndex]):
            for old_attribute in list(data_train[i])[:-1]:
                row.append(old_attribute)
        for i, row in enumerate(lda_list[splitIndex:]):
            for old_attribute in list(data_test[i])[:-1]:
                row.append(old_attribute)

    for i, row in enumerate(lda_list):
        row.append(yTable[i])
    attributes_train = attributes + attributes_train
    attributes_test  = attributes + attributes_test
    attributes_train.append(('class' , list(set(yTable))))
    attributes_test.append(('class' , list(set(yTable))))

    data_dict_train = {}
    data_dict_train['attributes'] = attributes_train
    data_dict_train['data'] = lda_list[:splitIndex]
    data_dict_train['description'] = u''
    data_dict_train['relation'] = relation_name

    data_dict_test = {}
    data_dict_test['attributes'] = attributes_test
    data_dict_test['data'] = lda_list[splitIndex:]
    data_dict_test['description'] = u''
    data_dict_test['relation'] = relation_name

    return {'test': arff.dumps(data_dict_test), 'train': arff.dumps(data_dict_train)}

    






