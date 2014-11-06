'''
Subgroup discovery library functions.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
import json
import logging
try:
    from SubgroupDiscovery.SD_learner_classifier import SD_learner
    from SubgroupDiscovery.SDRule import SDRules
    import Orange
except:
    print logging.warning('Could not import subgroup discovery orange package.')

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
    ruleset_name = postdata['ruleset_name'+widget_id][0]
    if not ruleset_name:
        ruleset_name = '%s rule set' % alg
    params = {'name' : alg, 'algorithm' : alg}
    for param in SubgroupDiscovery.algorithms[alg]:
        value = postdata[param+widget_id][0]
        if value != '':
            params[param] = SubgroupDiscovery.parameter_types[param](value)
    learner = SD_learner(**params)
    classifier = learner(data)
    rules = classifier.getRules(classValue)
    rules.algorithmName = ruleset_name
    # add id's
    for i, rule in enumerate(rules.rules):
        rule.id = i
    return {'rules' : rules, 'classifier' : classifier, 'learner': learner}

def subgroup_bar_visualization(input_dict):
    return {'rules' : None}

def subgroup_roc_visualization(input_dict):
    return {'rules' : []}

def subgroup_measures(input_dict):
    return {'rules': []}

def select_subgroups(input_dict):
    return {'sel_rules' : []}

def select_subgroups_finished(postdata, input_dict, output_dict):
    sd_rules = input_dict['rules']
    selected_subgroups = [int(i) for i in json.loads(postdata['selected_subgroup_ids'][0])]
    rules = SDRules(filter(lambda r: r.id in selected_subgroups, sd_rules.rules), sd_rules.targetClassRule, sd_rules.algorithmName)
    return {'sel_rules' : rules}

def query_with_subgroups(input_dict):
    data = input_dict['data']
    sd_rules = input_dict['rules']
    full_set = set([ex for ex in data])
    subset = set()
    for rule in sd_rules.rules:
        subset = subset.union(rule.filter(data))
    subset_table = Orange.data.Table(data.domain)
    remaining_table = Orange.data.Table(data.domain)
    for ex in subset:
        subset_table.append(ex)
    for ex in full_set.difference(subset):
        remaining_table.append(ex)
    return {'data' : subset_table, 'remaining_data' : remaining_table}

def table_from_sets(input_dict):
    return {'merged_data' : None}

def table_from_sets_finished(postdata, input_dict, output_dict):
    tables = input_dict['data']
    widget_id = postdata['widget_id'][0]
    class_name = str(postdata['class_name'+widget_id][0])
    class_values = [str(postdata['class'+str(i)+widget_id][0]) for i in range(len(tables))]
    replace = postdata.get('replace'+widget_id, [False])[0]
    replace = replace == 'on'
    class_var = Orange.feature.Discrete(class_name, values = class_values)
    class_table = Orange.data.Table(Orange.data.Domain([class_var]))
    # Check if the domains are the same
    domain = tables[0].domain
    for table in tables:
        if table.domain.variables != domain.variables:
            print table.domain, domain
            raise Exception('The domains of tables do not match!')
    for i, table in enumerate(tables):
        for ex in table:
            class_table.append(Orange.data.Instance(class_table.domain, [class_values[i]]))
    # All examples
    examples = Orange.data.Table(tables[0])
    for table in tables[1:]:
        examples.extend(table)
    old_table = examples
    if replace:
        if not examples.domain.classVar:
            raise Exception('No class variable present.')
        else:
            old_domain = list(examples.domain)
            old_domain.remove(examples.domain.classVar)
            old_table = Orange.data.Table(Orange.data.Domain(old_domain, examples.domain), examples)
    return {'merged_data' : Orange.data.Table([old_table, class_table])}
