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
    for row in arff.loads(arff_file):
        if attr_name:
            attr = getattr(row, attr_name)
        else:
            # Default to last row value
            attr = row[len(row)-1]
        attr_list.append(attr)
    return {'attr_list': attr_list}
