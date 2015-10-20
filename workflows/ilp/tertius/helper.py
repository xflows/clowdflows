import os
import shutil

def handle_files(fct_file_path,test_file_path, url):
    if os.path.normpath(fct_file_path) != os.path.normpath(url + '.fct'):
        shutil.copy(os.path.normpath(fct_file_path),os.path.normpath(url + '.fct'));

    if test_file_path and os.path.normpath(test_file_path) != os.path.normpath(url + '.test'):
        shutil.copy(os.path.normpath(test_file_path), os.path.normpath(url + '.test'));

    if not os.path.isfile(os.path.normpath(url + '.fct')) or not os.path.isfile(os.path.normpath(url + '.prd')):
        raise Exception('Prd or fct file missing')

def create_attribute_list(attribute_str):
    if attribute_str.strip():
        attribute_list = attribute_str.split(',')
        if attribute_list:       
            return [str(e) for e in ' '.join(map(lambda field: '-d ' + str(field), attribute_list)).split(' ')]
    return ''