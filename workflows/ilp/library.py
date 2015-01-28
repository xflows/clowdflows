import re
import json
import tempfile
from string import ascii_lowercase as chars
from random import choice

from aleph import Aleph
from rsd import RSD
from wordification import Wordification
from treeliker import TreeLiker
from security import check_input

from services.webservice import WebService

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
    settings = input_dict.get('settings',None)
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
    target_table = input_dict.get('target_table',None)
    other_tables = input_dict.get('other_tables', None)
    measure = input_dict.get('measure', None)
    context = input_dict.get('context', None)
    word_att_length = int(input_dict.get('f_ngram_size', 1))

    for _ in range(1):
        wordification = Wordification(target_table,other_tables,context,word_att_length)
        wordification.run(1)
        wordification.calculate_tf_idfs(measure)
        #wordification.prune(50)
        #wordification.to_arff()

    if 1==0:
        from wordification import Wordification_features_test
        wft=Wordification_features_test(target_table,other_tables,context)
        wft.print_results()
    return {'arff' : wordification.to_arff(),'corpus': wordification.wordify()}


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
