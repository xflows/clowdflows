# -*- coding: cp1252 -*-
import csv
import sys
from xml.dom import minidom






def get_attributes(file_xml):
    out = []
    dom1 = minidom.parse(file_xml)
    for node in dom1.getElementsByTagName('attribute'):
        out.append({
            'name': node.getAttribute('name') ,
            'atype': node.getAttribute('atype'),
            'format':node.getAttribute('format'),
            'skip':node.getAttribute('skip')

        })
    #print out
    return out

def get_relation(file_xml):
    dom1 = minidom.parse(file_xml)
    out=''
    delimiter=''
    for node in dom1.getElementsByTagName('csv'):
        out=node.getAttribute('name')
        delimiter=node.getAttribute('delimiter');
    if(len(delimiter)==0):
        delimiter=';';
    print delimiter   
    return out, delimiter


class csv_arff_converter:
    
    def __init__(self,csv_file, attribute_file, file_out):
        self.csv_file = csv_file
        self.attribute_file = attribute_file
        self.file_out = file_out

    def run(self):

        classes = []
        
        #read attribute
        self.relation_name, self.delimiter = get_relation(attribute_file)
        attributes_list = get_attributes(attribute_file)
        arff_data = '@RELATION ' + self.relation_name + '\n\n'
         

        for i in attributes_list:
            if (i['skip'] != 'yes'):
                arff_data += '@ATTRIBUTE '+i['name']+' ' + i['atype']
                if (i['atype']=='date'):
                    arff_data += ' '+i['format']
                if (i['atype']=='class'):
                    arff_data += ' (#@#'+i['name'] + '#@#)'
                
                arff_data +='\n'
                classes.append('')
             


        arff_data += '\n@DATA\n'
        print classes 
        #open csv
        reader = csv.reader(open(self.csv_file), delimiter=self.delimiter, quoting=csv.QUOTE_NONE)

        rnum = 0     


        for row in reader:
            #print row
            buff = ''
            pos = 0
            #print len(row)
            #occhio alla lunghezza riga
            for j in range(0, len(row)-1):
                field = row[j]
                
                if(attributes_list[pos]['skip'] != 'yes'):
                
                    if (pos > 0):
                        buff += ','
                    if(attributes_list[pos]['atype'] == 'string'):
                        field = "'" + field + "'"
                    buff += field
                    #se è una classe raccolgo i valori
                    if(attributes_list[pos]['atype'] == 'class'):
                        if (rnum > 0):
                            classes[pos]+= ','+ field
                        else:
                            classes[pos]+=  field
                        
                pos += 1
            buff += '\n'
            arff_data += buff
            
            rnum += 1

        
        pos = 0
        for a in classes:
            j = a.split(',')
            
            un = list(set(j))
            #print un
            if (len(un) > 0):
                this_replacement =  ",".join(un)
                #print this_replacement
                old_text = '#@#'+ attributes_list[pos]['name'] + '#@#'
                #print old_text
                arff_data = arff_data.replace(old_text, this_replacement)
            pos += 1

        #print arff_data
        a = open(self.file_out, 'w')
        a.write(arff_data)
        a.close()
        
                
                



if __name__ == "__main__":
    #csv_file = sys.argv[1]
    #attribute_file = sys.argv[2]
    csv_file = './test_csv2arff/test_dataset_1.csv'
    attribute_file = './test_csv2arff/test_dataset_1.att'
        
    instance = csv_arff_converter(csv_file, attribute_file, './test_csv2arff/output.arff')
    instance.run()
