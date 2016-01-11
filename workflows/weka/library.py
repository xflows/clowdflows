import StringIO
import arff
from services.webservice import WebService


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

def weka_arff_to_weka_instances(input_dict):
    '''
    Reads a dataset into a format suitable for WEKA methods
    '''
    import jpype as jp
    import common

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
    import jpype as jp
    import common

    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    instances = common.deserialize_weka_object(input_dict['instances'])

    arff = instances.toString()

    return {'arff':arff}
