import arff
from services.webservice import WebService
from scipy.io import arff as scipy_arff
from cStringIO import StringIO
import numpy
from sklearn.decomposition import LatentDirichletAllocation


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
    arff_file_fit = StringIO(input_dict['fit'])
    arff_file_transform = StringIO(input_dict['transform'])
    data_fit, meta_fit = scipy_arff.loadarff(arff_file_fit)
    data_transform, meta_transform = scipy_arff.loadarff(arff_file_transform)
    n_topics = int(input_dict['n_topics'])
    n_iter = int(input_dict['n_iter'])
    relation_name = input_dict['relation_name']
    random_state = int(input_dict['random_state'])
    keep_original_dimensions = input_dict['keep_original']
    model = LatentDirichletAllocation(n_topics=n_topics, max_iter=n_iter, random_state=random_state)
    dataTable_fit = []
    dataTable_transform = []
    yTable = []
    for instance in data_fit:
        row = []
        for attribute in instance:
            row.append(attribute)
        dataTable_fit.append(row[:-1])


    meta_transform = list(meta_transform)
    for instance in data_transform:
        row = []
        '''print '\n\ntransform'
        print list(meta_transform)
        print instance'''
        for attribute in meta_fit:
            try:
                idx = meta_transform.index(attribute)
                row.append(instance[idx])
            except:
                row.append(0.0)
        '''print 'fit'
        print list(meta_fit)
        print row'''
        dataTable_transform.append(row[:-1])
        yTable.append(instance[-1])

    train = numpy.array(dataTable_fit)
    test = numpy.array(dataTable_transform)
    model= model.fit(train)
    model= model.transform(test)
    lda_list = model.tolist()
    attributes = []
    for i in range(n_topics):
        attributes.append(('topic_' + str(i), u'REAL'))
    if keep_original_dimensions:
        for attribute in meta_transform[:-1]:
            attributes.append((attribute, u'REAL'))
        for i, row in enumerate(lda_list):
            print row
            for old_attribute in list(data_transform[i])[:-1]:
                row.append(old_attribute)

    for i, row in enumerate(lda_list):
        row.append(yTable[i])
    attributes.append(('class' , list(set(yTable))))

    data_dict = {}
    data_dict['attributes'] = attributes
    data_dict['data'] = lda_list
    data_dict['description'] = u''
    data_dict['relation'] = relation_name


   

    return {'arff': arff.dumps(data_dict)}

    






