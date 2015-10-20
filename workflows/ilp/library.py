import re
import json
import tempfile

from aleph import Aleph
from rsd import RSD
from wordification import Wordification
from treeliker import TreeLiker
from security import check_input

from services.webservice import WebService

import shutil
import os
from subprocess import Popen, PIPE
import time


def ilp_aleph(input_dict):
    aleph = Aleph()
    settings = input_dict['settings']
    mode = input_dict['mode']
    pos = input_dict['pos']
    neg = input_dict['neg']
    b = input_dict['b']
    # Parse settings provided via file
    if settings:
        aleph.settingsAsFacts(settings)
    # Parse settings provided as parameters (these have higher priority)
    for setting, def_val in Aleph.ESSENTIAL_PARAMS.items():
        aleph.set(setting, input_dict.get(setting, def_val))
    # Check for illegal predicates
    for pl_script in [b, pos, neg]:
        check_input(pl_script)
    # Run aleph
    results = aleph.induce(mode, pos, neg, b)
    return {'theory': results[0], 'features': results[1]}

def ilp_rsd(input_dict):
    rsd = RSD()
    settings = input_dict.get('settings', None)
    pos = input_dict.get('pos', None)
    neg = input_dict.get('neg', None)
    examples = input_dict.get('examples', None)
    b = input_dict['b']
    subgroups = input_dict['subgroups'] == 'true'
    # Parse settings
    if settings:
        rsd.settingsAsFacts(settings)
    # Parse settings provided as parameters (these have higher priority)
    for setting, def_val in RSD.ESSENTIAL_PARAMS.items():
        rsd.set(setting, input_dict.get(setting, def_val))
    # Check for illegal predicates
    for pl_script in [b, pos, neg, examples]:
        check_input(pl_script)
    # Run rsd
    features, arff, rules = rsd.induce(b, examples=examples, pos=pos, neg=neg, cn2sd=subgroups)
    return {'features' : features, 'arff' : arff, 'rules' : rules}


def ilp_sdmsegs_rule_viewer(input_dict):
    return {}

def ilp_sdmaleph(input_dict):
    import orange
    ws = WebService('http://vihar.ijs.si:8097', 3600)
    data = input_dict.get('examples')
    if isinstance(data, orange.ExampleTable):
        with tempfile.NamedTemporaryFile(suffix='.tab', delete=True) as f:
            data.save(f.name)
            examples = f.read()
    elif isinstance(data, list):
        examples = json.dumps(data)
    elif isinstance(data, str):
        examples = data
    else:
        raise Exception('Illegal examples format. \
                         Supported formats: str, list or Orange')
    response = ws.client.sdmaleph(
        examples=examples,
        mapping=input_dict.get('mapping'),
        ontologies=[{'ontology' : ontology} for ontology in input_dict.get('ontology')],
        relations=[{'relation' : relation} for relation in input_dict.get('relation')],
        posClassVal=input_dict.get('posClassVal') if input_dict.get('posClassVal') != '' else None,
        cutoff=input_dict.get('cutoff') if input_dict.get('cutoff') != '' else None,
        minPos=input_dict.get('minPos') if input_dict.get('minPos') != '' else None,
        noise=input_dict.get('noise') if input_dict.get('noise') != '' else None,
        clauseLen=input_dict.get('clauseLen') if input_dict.get('clauseLen') != '' else None,
        dataFormat=input_dict.get('dataFormat') if input_dict.get('dataFormat') != '' else None
    )
    return {'theory' : response['theory']}


def ilp_wordification(input_dict):
    target_table = input_dict.get('target_table', None)
    other_tables = input_dict.get('other_tables', None)
    weighting_measure = input_dict.get('weighting_measure', 'tfidf')
    context = input_dict.get('context', None)
    word_att_length = int(input_dict.get('f_ngram_size', 1))
    idf = input_dict.get('idf', None)

    for _ in range(1):
        wordification = Wordification(target_table, other_tables, context, word_att_length, idf)
        wordification.run(1)
        wordification.calculate_tf_idfs(weighting_measure)
        # wordification.prune(50)
        # wordification.to_arff()

    if 1 == 0:
        from wordification import Wordification_features_test
        wft = Wordification_features_test(target_table, other_tables, context)
        wft.print_results()
    return {'arff' : wordification.to_arff(), 'corpus': wordification.wordify(), 'idf':wordification.idf}


def ilp_treeliker(input_dict):
    template = input_dict['template']
    dataset = input_dict['dataset']
    settings = {
        'algorithm': input_dict.get('algorithm'),
        'minimum_frequency': input_dict.get('minimum_frequency'),
        'covered_class': input_dict.get('covered_class'),
        'maximum_size': input_dict.get('maximum_size'),
        'use_sampling': input_dict.get('use_sampling'),
        'sample_size': input_dict.get('sample_size'),
        'max_degree': input_dict.get('max_degree')
    }
    treeliker = TreeLiker(dataset, template, settings=settings)
    arff_train, arff_test = treeliker.run()
    return {'arff': arff_train, 'treeliker': treeliker}

def ilp_cardinalization(input_dict):
    return cardinalization(input_dict,False)

def ilp_quantiles(input_dict):
    return cardinalization(input_dict,False)

def ilp_relaggs(input_dict):
    return cardinalization(input_dict,True)

def cardinalization(input_dict,is_relaggs):
    output_dict = {}
    excluded_fields = parse_excluded_fields(input_dict['context'])
    args_list = ['java', '-Xmx512m', '-jar', 'proper/properLauncher.jar']
    if is_relaggs:
        args_list += ['-relaggs']
    else:
        args_list += ['-cardinalizer']

    result_table = '_%s_%s' % (('relaggs' if is_relaggs else ('quantiles' if 'discretize_parts' in input_dict else 'cardinalize' )), int(round(time.time() * 1000)) )
    #progress bar issue
    #result_table = '_%s_%s_%s' % (('relaggs' if is_relaggs else ('quantiles' if 'discretize_parts' in input_dict else 'cardinalize' )), str(widget.workflow_id), str(widget.id))

    args_list += [
            '-use_foreign_keys',
            '-associated_tables', ','.join(set(input_dict['context'].tables).difference(input_dict['context'].target_table)),
            '-user',input_dict['context'].connection.user,
            '-password', input_dict['context'].connection.password, 
            '-result_table', result_table, 
            '-driver', input_dict['context'].connection.dal.get_driver_name(),
            '-exclude_fields', excluded_fields,
            '-field',input_dict['context'].target_att,
            '-url', input_dict['context'].connection.dal.get_jdbc_prefix() + input_dict['context'].connection.host + '/',
            '-database', input_dict['context'].connection.database,                    
            '-table', input_dict['context'].target_table]
    
    try:
        args_list += ['-discretize', '1','-discretize-parts', input_dict['discretize_parts'] ]
    except KeyError:
        pass

    p = Popen(args_list,cwd=os.path.dirname(os.path.abspath(__file__)), stdout=PIPE)
    stdout_str, stderr_str = p.communicate()
    
    output_dict['context'] = input_dict['context'].change_table(result_table)        
    return output_dict

def parse_excluded_fields(context):   
    excluded_fields = set()
    for table in context.tables:
        excluded_set = set(context.all_cols[table]).difference(set(context.col_vals[table].keys()))
        if excluded_set:
            excluded_fields.add(','.join(map(lambda field: table + '.' + str(field), excluded_set)))
    return ','.join(excluded_fields)

def ilp_1bc(input_dict):
    output_dict = {}
       
    url = os.path.normpath(re.sub('\..*$', '', input_dict['prd_file']))
    
    handle_files(input_dict, url)
              
    args_list = [os.path.join(os.path.dirname(os.path.abspath(__file__)),os.path.join('tertius','1BC.exe' if os.name == 'nt' else '1BC')), '-r','LANGUAGE']
    args_list += ['-m', '512']

    try:
        max_literal = int(input_dict['max_literal'])
    except ValueError:
        raise Exception('Max Literal should be an integer')
    
    try:
        max_variable = int(input_dict['max_variable'])
    except ValueError:
        raise Exception('Max Variable should be an integer')
           
    try:
        cross_number = int(input_dict['cross_number'])
    except ValueError:
        pass
    else:
        args_list += ['-cross', str(cross_number)]
        
    try:
        srand = int(input_dict['srand'])
    except ValueError:
        pass
    else:
        args_list += ['-srand', str(srand)]
        
    try:
        roc_nb_folds = int(input_dict['roc_nb_folds'])
    except ValueError:
        pass
    else:
        if roc_nb_folds >= 0:
            args_list += ['-o', str(roc_nb_folds)]   
            
    if input_dict['load_part_inc']:
        args_list += ['-i']                 

    att_list = create_attribute_list(input_dict['attribute_list'])
    if att_list:
        args_list += att_list   
    
    args_list += ['class']
    
    args_list += [str(max_literal) + '/' + str(max_variable), os.path.relpath(url, os.path.dirname(os.path.abspath(__file__)))]    
    p = Popen(args_list,cwd=os.path.dirname(os.path.abspath(__file__)), stdout=PIPE)   
    stdout_str, stderr_str = p.communicate()      

    with open(url + '.res') as f:
        res_file = f.read()

    output_dict['results'] = res_file
        
    return output_dict

def ilp_tertius(input_dict):
    output_dict = {}

    url = os.path.normpath(re.sub('\..*$', '', input_dict['prd_file']))
    
    handle_files(input_dict, url)
        
    args_list = [os.path.join(os.path.dirname(os.path.abspath(__file__)),os.path.join('tertius','tertius.exe' if os.name == 'nt' else 'tertius'))]
    args_list += ['-m', '512']
    
    try:
        max_literal = int(input_dict['max_literal'])
    except ValueError:
        raise Exception('Max Literal should be an integer')
    
    try:
        max_variable = int(input_dict['max_variable'])
    except ValueError:
        raise Exception('Max Variable should be an integer')

    try:
        struct_nb_properties = int(input_dict['struct_nb_properties'])
    except ValueError:
        pass
    else:
        if struct_nb_properties >= 0:
            args_list += ['-struct', str(struct_nb_properties)]   

    try:
        nb_results = int(input_dict['nb_results'])
    except ValueError:
        nb_results = -1
    else:
        if nb_results > 0:
            args_list += ['-k', str(nb_results)]   
         
    if nb_results < 0:
        try:
            conf_thres = int(input_dict['conf_thres'])
        except ValueError:
            conf_thres = -1
        else:
            if conf_thres > 0:
                args_list += ['-c', str(conf_thres)]   
                
    if input_dict['count_bottom_up']:
        args_list += ['-cbu']                       

    if input_dict['sat_clauses']:
        args_list += ['-sat'] 
        
    try:
        noise_percent_thres = float(input_dict['noise_percent_thres'])
    except ValueError:
        pass
    else:
        if noise_percent_thres >= 0:
            args_list += ['-n', str(noise_percent_thres)]   
            
    if input_dict['lang_horn'] and input_dict['lang_pos_class']:
        args_list += ['-b', 'horn_pos_class']   
    elif input_dict['lang_horn']:
        args_list += ['-b', 'horn']   
    elif input_dict['lang_pos_class']:
        args_list += ['-b', 'pos_class']   
        
    att_list = create_attribute_list(input_dict['attribute_list'])
    if att_list:
        args_list += att_list  
                            
    args_list += [str(max_literal) + '/' + str(max_variable),os.path.relpath(url, os.path.dirname(os.path.abspath(__file__)))]
    p = Popen(args_list,cwd=os.path.dirname(os.path.abspath(__file__)), stdout=PIPE)   
    stdout_str, stderr_str = p.communicate()       

    with open(url+'.res') as f:
        res_file = f.read()

    output_dict['results'] = res_file
                   
    return output_dict

def handle_files(input_dict, url):
    if os.path.normpath(input_dict['fct_file']) != os.path.normpath(url + '.fct'):
        shutil.copy(os.path.normpath(input_dict['fct_file']),os.path.normpath(url + '.fct'));

    if input_dict['test_file'] and os.path.normpath(input_dict['test_file']) != os.path.normpath(url + '.test'):
        shutil.copy(os.path.normpath(input_dict['test_file']), os.path.normpath(url + '.test'));

    if not os.path.isfile(os.path.normpath(url + '.fct')) or not os.path.isfile(os.path.normpath(url + '.prd')):
        raise Exception('Prd or fct file missing')
    
def create_attribute_list(attribute_str):
    if attribute_str.strip():
        attribute_list = attribute_str.split(',')
        if attribute_list:       
            return [str(e) for e in ' '.join(map(lambda field: '-d ' + str(field), attribute_list)).split(' ')]
    return ''