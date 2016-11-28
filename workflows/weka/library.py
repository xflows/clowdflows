import arff
from services.webservice import WebService
from scipy.io import arff as scipy_arff
from cStringIO import StringIO
import lda
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
    arff_file = StringIO(input_dict['arff'])
    data, meta = scipy_arff.loadarff(arff_file)
    n_topics = int(input_dict['n_topics'])
    n_iter = int(input_dict['n_iter'])
    relation_name = input_dict['relation_name']
    random_state = int(input_dict['random_state'])
    model = LatentDirichletAllocation(n_topics=n_topics, max_iter=n_iter, random_state=random_state)
    dataTable = []
    yTable = []
    for instance in data:
        row = []
        for attribute in instance:
            try:
                row.append(attribute)
            except:
                pass
        dataTable.append(row[:-1])
        yTable.append(instance[-1])
    
    X = numpy.array(dataTable)
    fitted_model= model.fit_transform(X)
    lda_list = fitted_model.tolist()
    for i, row in enumerate(lda_list):
        row.append(yTable[i])
    attributes = []
    for i in range(n_topics):
        attributes.append(('topic_' + str(i), u'REAL'))
    attributes.append(('class' , list(set(yTable))))
    data_dict = {}
    data_dict['attributes'] = attributes
    data_dict['data'] = lda_list
    data_dict['description'] = u''
    data_dict['relation'] = relation_name

    return {'arff': arff.dumps(data_dict)}

    






