'''
Subgroup discovery library functions.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
import json
from SubgroupDiscovery.SD_learner_classifier import SD_learner
from SubgroupDiscovery.SDRule import SDRules

class SubgroupDiscovery:
    # Available algorithms and their parameters
    algorithms = {
        'SD' : ['minSupport', 'g', 'beamWidth', 'max_rules'],
        'SD-Preprocess' : ['minSupport', 'g', 'beamWidth', 'max_rules'],
        'Apriori-SD' : ['minSupport', 'minConfidence', 'k', 'max_rules'],
        'CN2-SD' : ['k', 'max_rules']
    }
    # Parameter default values and descriptions
    parameter_info = {
        'minSupport' : (0.1, 'Minimum support'), 
        'minConfidence' : (0.7, 'Minimum confidence'),
        'g' : (1.0, 'Generalization parameter'),
        'beamWidth' : (5, 'Beam width'), 
        'k' : (4, 'Number of times covered before removed'), 
        'max_rules' : (0, 'Maximum number of subgroups (0=no limitation)')
    }
    # Parameter data types
    parameter_types = {
        'minSupport' : float, 
        'minConfidence' : float, 
        'g' : float,
        'beamWidth' : int,
        'k' : int,
        'max_rules' : int,
    }

def build_subgroups(input_dict):
    return {'rules' : None, 'classifier' : None}

def build_subgroups_finished(postdata, input_dict, output_dict):
    data = input_dict['data']
    widget_id = postdata['widget_id'][0]
    alg = postdata['algorithm'+widget_id][0]
    classValue = str(postdata['class'+widget_id][0])
    params = {'name' : alg, 'algorithm' : alg}
    for param in SubgroupDiscovery.algorithms[alg]:
        value = postdata[param+widget_id][0]
        if value != '':
            params[param] = SubgroupDiscovery.parameter_types[param](value)
    learner = SD_learner(**params)
    classifier = learner(data)
    rules = classifier.getRules(classValue)
    # add id's
    for i, rule in enumerate(rules.rules):
        rule.id = i
    return {'rules' : rules, 'classifier' : learner}

def subgroup_bar_visualization(input_dict):
    return {'rules' : None}

def subgroup_roc_visualization(input_dict):
    return {'rules' : []}

def select_subgroups(input_dict):
    return {'sel_rules' : []}

def select_subgroups_finished(postdata, input_dict, output_dict):
    sd_rules = input_dict['rules']
    selected_subgroups = [int(i) for i in json.loads(postdata['selected_subgroup_ids'][0])]
    rules = SDRules(filter(lambda r: r.id in selected_subgroups, sd_rules.rules), sd_rules.targetClassRule, sd_rules.algorithmName)
    return {'sel_rules' : rules}