import os
import shutil
import re

def handle_files(fct_file_path, url, test_file_path=None):
    if os.path.normpath(fct_file_path) != os.path.normpath(url + '.fct'):
        shutil.copy(os.path.normpath(fct_file_path),os.path.normpath(url + '.fct'));

    if test_file_path and os.path.normpath(test_file_path) != os.path.normpath(url + '.test'):
        shutil.copy(os.path.normpath(test_file_path), os.path.normpath(url + '.test'));

    if not os.path.isfile(os.path.normpath(url + '.fct')) or not os.path.isfile(os.path.normpath(url + '.prd')):
        raise Exception('Prd or fct file missing')

def create_attribute_list(attribute_str):        
    if attribute_str: # e.g. : '  col1    5  eqb , col2 6 eqb   '
        attribute_str = ' '.join(attribute_str.split()) # e.g. : 'col1 5 eqb , col2 6 eqb'
        attribute_list = attribute_str.split(',') # e.g. : ['col1 5 eqb ',' col2 6 eqb']
        if attribute_list:
            #map & strip e.g. : ['-d col1 5 eqb','-d col2 6 eqb']
            #join e.g. : '-d col1 5 eqb -d col2 6 eqb'
            res = check_attributes(' '.join(map(lambda field: '-d ' + str(field).strip(), attribute_list)))
            if res is None:
                return ''
            else:
                #split e.g. : ['-d','col1','5','eqb','-d','col2','6','eqb']
                return res.split(' ')
    return ''

def check_attributes(attribute_str):
    print attribute_str
    if re.search('^(-d \w+ \d+ (eqb|sdm))( -d \w+ \d+ (eqb|sdm))*$',attribute_str):
        return attribute_str
    else :
        raise Exception('Attribute list is not correct')
    return None