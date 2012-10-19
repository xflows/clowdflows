from workflows.security import safeOpen
import nlp
import cPickle

def test_interaction(input_dict):
    return input_dict

def create_list(input_dict):
    return input_dict
    
def add_multiple(input_dict):
    output_dict = {}
    output_dict['sum'] = 0
    for i in input_dict['integer']:
        output_dict['sum'] = int(i)+output_dict['sum']
    return output_dict

def delay(input_dict,widget):
    widget.progress=0
    widget.save()
    timeleft = int(input_dict['time'])
    i = 0
    import time
    import math
    while i<timeleft:
        time.sleep(1)
        i=i+1
        widget.progress = math.floor(((i*1.0)/timeleft)*100)
        widget.save()
    widget.progress=100
    widget.save()
    output_dict = {}
    output_dict['data'] = input_dict['data']
    return output_dict

def load_file(input_dict):
    return input_dict
    
def file_to_string(input_dict):
    f = safeOpen(input_dict['file'])
    output_dict = {}
    output_dict['string']=f.read()
    return output_dict

def load_to_string(input_dict):
    '''
    Opens the file and reads its contents into a string.
    '''
    f = safeOpen(input_dict['file'])
    output_dict = {}
    output_dict['string']=f.read()
    return output_dict

def pickle_object(input_dict):
    '''
    Serializes the input object.
    '''
    pkl_obj = cPickle.dumps(input_dict['object'])
    output_dict = {}
    output_dict['pickled_object'] = pkl_obj
    return output_dict

def unpickle_object(input_dict):
    '''
    Serializes the input object.
    '''
    obj = cPickle.loads(str(input_dict['pickled_object']))
    output_dict = {}
    output_dict['object'] = obj
    return output_dict

def call_webservice(input_dict):
    from services.webservice import WebService
    ws = WebService(input_dict['wsdl'],float(input_dict['timeout']))
    selected_method = {}
    for method in ws.methods:
        if method['name']==input_dict['wsdl_method']:
            selected_method = method
    function_to_call = getattr(ws.client,selected_method['name'])
    ws_dict = {}
    for i in selected_method['inputs']:
        try:
            ws_dict[i['name']]=input_dict[i['name']]
            if ws_dict[i['name']] is None:
                pass
            if i['type'] == bool:
                if input_dict[i['name']]=="true":
                    ws_dict[i['name']]=1
                else:
                    ws_dict[i['name']]=0
            if ws_dict[i['name']] == '':
                if input_dict['sendemptystrings']=="true":
                    ws_dict[i['name']] = ''
                else:
                    ws_dict.pop(i['name'])
        except Exception as e: 
            print e
            ws_dict[i['name']]=''
    print ws_dict
    results = function_to_call(**ws_dict)
    output_dict=results
    return output_dict

def multiply_integers(input_dict):
    product = 1
    for i in input_dict['integers']:
        product = product*int(i)
    output_dict={'integer':product}
    return output_dict

def filter_integers(input_dict):
    return input_dict
    
def filter_integers_post(postdata,input_dict,output_dict):
    try:
        output_dict['integers'] = postdata['integer']
    except:
        pass
    return output_dict

def create_integer(input_dict):
    output_dict = {}
    output_dict['integer'] = input_dict['integer']
    return output_dict
    
def create_string(input_dict):
    return input_dict  
    
def concatenate_strings(input_dict):
    output_dict = {}
    j = len(input_dict['strings'])
    for i in range(j):
        input_dict['strings'][i]=str(input_dict['strings'][i])
    output_dict['string'] = input_dict['delimiter'].join(input_dict['strings'])
    return output_dict
    
def display_string(input_dict):
    return {}

def add_integers(input_dict):
    output_dict = {}
    output_dict['integer'] = int(input_dict['integer1'])+int(input_dict['integer2'])
    return output_dict

def object_viewer(input_dict):
    return {}

def table_viewer(input_dict):
    return {}

def subtract_integers(input_dict):
    output_dict = {}
    output_dict['integer'] = int(input_dict['integer1'])-int(input_dict['integer2'])
    return output_dict
    
def create_range(input_dict):
    output_dict = {}
    output_dict['rangeout'] = range(int(input_dict['n_range']))
    return output_dict

def select_attrs(input_dict):
    return input_dict

def select_attrs_post(postdata, input_dict, output_dict):
    import Orange
    
    data = Orange.data.Table(input_dict['data'])
    
    new_attrs = []
    for name in postdata['attrs']:
        new_attrs.append(str(name))
    
    try:
        new_attrs.append(str(postdata['ca'][0]))
        class_attr = True
    except:
        class_attr = False

    new_domain = Orange.data.Domain(new_attrs, class_attr, data.domain)

    try:
        for meta in postdata['ma']:
            if data.domain.has_meta(str(meta)):
                new_domain.addmeta(Orange.feature.Descriptor.new_meta_id(), data.domain.getmeta(str(meta)))
            else:
                new_domain.add_meta(Orange.feature.Descriptor.new_meta_id(), data.domain[str(meta)])
    except:
        pass    

    new_data = Orange.data.Table(new_domain, data)

    output_dict = {'data':new_data}
    return output_dict

def select_data(input_dict):
    return input_dict

def build_filter(val, attr, data):
    import Orange

    pos = 0

    try:
        pos = data.domain.meta_id(attr)
    except Exception, e:
        pos = data.domain.variables.index(attr)

    if val['operator'] == ">":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                ref = float(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Greater
            )
        )
    elif val['operator'] == "<":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                ref = float(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Less
            )
        )
    elif val['operator'] == "=":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                ref = float(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Equal
            )
        )
    elif val['operator'] == "<=":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                ref = float(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.LessEqual
            )
        )
    elif val['operator'] == ">=":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                ref = float(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.GreaterEqual
            )
        )
    elif val['operator'] == "between":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                min = float(val['values'][0]),
                max = float(val['values'][1]),
                oper = Orange.data.filter.ValueFilter.Between
            )
        )
    elif val['operator'] == "outside":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                min = float(val['values'][0]),
                max = float(val['values'][1]),
                oper = Orange.data.filter.ValueFilter.Outside
            )
        )
    elif val['operator'] in ["equals", "in"]:
        vals=[]
        for v in val['values']:
            vals.append(Orange.data.Value(attr, str(v)))
        return(
            Orange.data.filter.ValueFilterDiscrete(
                position = pos,
                values=vals
            )
        )
    elif val['operator'] == "s<":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Less,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "s>":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Greater,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "s=":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Equal,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "s<=":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.LessEqual,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "s>=":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.GreaterEqual,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "sbetween":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                min = str(val['values'][0]),
                max = str(val['values'][1]),
                oper = Orange.data.filter.ValueFilter.Between,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "soutside":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                min = str(val['values'][0]),
                max = str(val['values'][1]),
                oper = Orange.data.filter.ValueFilter.Outside,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "scontains":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Contains,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "snot contains":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.NotContains,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "sbegins with":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.BeginsWith,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "sends with":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.EndsWith,
                case_sensitive = bool(val['case'])
            )
        )

def select_data_post(postdata, input_dict, output_dict):
    import Orange, json

    data = input_dict['data']

    try:
        conditions = json.loads(str(postdata['conditions'][0]))

        for c in conditions['conditions']:
            if c['condition'][0]['operator'] in ["is defined", "sis defined"]:
                # if the operator is "is defined"

                fil = Orange.data.filter.IsDefined(domain = data.domain)
                for v in range(len(data.domain.variables)):
                    fil.check[int(v)] = 0
                    
                if c['negate']:
                    fil.negate = True

                fil.check[str(c['condition'][0]['attr'])] = 1

            else:
                fil = Orange.data.filter.Values()
                fil.domain = data.domain

                if c['negate']:
                    fil.negate = True

                if len(c['condition']) > 1:
                    fil.conjunction = False
                    for val in c['condition']:
                        attr = data.domain[str(val['attr'])]
                        fil.conditions.append(build_filter(val, attr, data))

                else:
                    for val in c['condition']:
                        attr = data.domain[str(val['attr'])]
                        fil.conditions.append(build_filter(val, attr, data))

            data = fil(data)
    except Exception, e:
        pass
    
    output_dict = {'data': data}
    return output_dict

def build_classifier(input_dict):
    learner = input_dict['learner']
    data = input_dict['data']

    classifier = learner(data)

    output_dict = {'classifier': classifier}

    return output_dict

def apply_classifier(input_dict):
    import Orange

    classifier = input_dict['classifier']
    data = input_dict['data']

    new_domain = Orange.data.Domain(data.domain, classifier(data[0]).variable)
    new_domain.add_metas(data.domain.get_metas())
    
    new_data = Orange.data.Table(new_domain, data)

    for i in range(len(data)):
        c = classifier(data[i])
        new_data[i][c.variable.name] = c

    output_dict = {'data':new_data}

    return output_dict

# ORANGE CLASSIFIERS (update imports when switching to new orange version)

def bayes(input_dict):
    import orange
    output_dict = {}
    output_dict['bayesout']= orange.BayesLearner(name = "Naive Bayes (Orange)", hovername="DOlgo ime bajesa")
    return output_dict

def knn(input_dict):
    import orange
    output_dict = {}
    output_dict['knnout']= orange.kNNLearner(name = "kNN (Orange)")
    return output_dict

def rules(input_dict):
    import orange
    output_dict = {}
    output_dict['rulesout']= orange.RuleLearner(name = "Rule Learner (Orange)")
    return output_dict
    
def cn2(input_dict):
    import orngCN2
    output_dict = {}
    output_dict['cn2out']= orngCN2.CN2Learner(name = "CN2 Learner (Orange)")
    return output_dict
    
def svm(input_dict):
    import orngSVM
    output_dict = {}
    output_dict['svmout']= orngSVM.SVMLearner(name = 'SVM (Orange)')
    return output_dict
    
def svmeasy(input_dict):
    import orngSVM
    output_dict = {}
    output_dict['svmeasyout']= orngSVM.SVMLearnerEasy(name='SVMEasy (Orange)')
    return output_dict
    
def class_tree(input_dict):
    import orange
    output_dict = {}
    output_dict['treeout']= orange.TreeLearner(name = "Classification Tree (Orange)")
    return output_dict
    
def c45_tree(input_dict):
    import orange
    output_dict = {}
    output_dict['c45out']= orange.C45Learner(name = "C4.5 Tree (Orange)")
    return output_dict  
    
def logreg(input_dict):
    import orange
    output_dict = {}
    output_dict['logregout']= orange.LogRegLearner(name = "Logistic Regression (Orange)")
    return output_dict
    
def majority_learner(input_dict):
    import orange
    output_dict = {}
    output_dict['majorout']= orange.MajorityLearner(name = "Majority Classifier (Orange)")
    return output_dict
    
def lookup_learner(input_dict):
    import orange
    output_dict = {}
    output_dict['lookupout']= orange.LookupLearner(name = "Lookup Classifier (Orange)")
    return output_dict

def random_forest(input_dict):
    from workflows.helpers import UnpicklableObject 
    output_dict = {}
    rfout = UnpicklableObject("orngEnsemble.RandomForestLearner(trees="+input_dict['n']+", name='RF"+str(input_dict['n'])+" (Orange)')")
    rfout.addimport("import orngEnsemble")
    output_dict['rfout']=rfout
    return output_dict    

# HARF (HIGH AGREEMENT RANDOM FOREST)

def harf(input_dict):
    #import orngRF_HARF
    from workflows.helpers import UnpicklableObject
    agrLevel = input_dict['agr_level']
    #data = input_dict['data']
    harfout = UnpicklableObject("orngRF_HARF.HARFLearner(agrLevel ="+agrLevel+", name='HARF-"+str(agrLevel)+"')")
    harfout.addimport("import orngRF_HARF")
    #harfLearner = orngRF_HARF.HARFLearner(agrLevel = agrLevel, name = "_HARF-"+agrLevel+"_")
    output_dict = {}
    output_dict['harfout']= harfout
    return output_dict
    
# CLASSIFICATION NOISE FILTER

def classification_filter(input_dict, widget):
    import noiseAlgorithms4lib    
    output_dict = {}
    output_dict['noise_dict']= noiseAlgorithms4lib.cfdecide(input_dict, widget)
    return output_dict    
    
def send_filename(input_dict):
    output_dict = {}
    output_dict['filename']=input_dict['fileloc'].strip('\"').replace('\\', '\\\\')
    return output_dict
    
def load_dataset(input_dict):
    import orange
    output_dict = {}
    output_dict['dataset'] = orange.ExampleTable(input_dict['file'])
    return output_dict
    
# SATURATION NOISE FILTER

def saturation_filter(input_dict, widget):
    import noiseAlgorithms4lib    
    output_dict = {}
    output_dict['noise_dict']= noiseAlgorithms4lib.saturation_type(input_dict, widget)
    return output_dict
    
# ENSEMBLE

def ensemble(input_dict):
    import math
    ens = {}
    data_inds = input_dict['data_inds']
    ens_type = input_dict['ens_type']
    # TODO ens_level = input_dict['ens_level']
    for item in data_inds:
        #det_by = item['detected_by']
        for i in item['inds']:
            if not ens.has_key(i):
                ens[i] = 1
            else:
                ens[i] += 1
    
    ens_out = {}
    ens_out['name'] = input_dict['ens_name']
    ens_out['inds'] = []
    n_algs = len(data_inds)
    print ens_type
    if ens_type == "consensus":
        ens_out['inds'] = sorted([x[0] for x in ens.items() if x[1] == n_algs])
    else: # majority
        ens_out['inds'] = sorted([x[0] for x in ens.items() if x[1] >= math.floor(n_algs/2+1)])
    
    output_dict = {}
    output_dict['ens_out'] = ens_out
    return output_dict

# NOISE RANK
    
def noiserank(input_dict):
    allnoise = {}
    data = input_dict['data']
    for item in input_dict['noise']:
        det_by = item['name']
        for i in item['inds']:
            if not allnoise.has_key(i):
                allnoise[i] = {}
                allnoise[i]['id'] = i
                allnoise[i]['class'] = data[int(i)].getclass().value
                allnoise[i]['by'] = []
            allnoise[i]['by'].append(det_by)
            print allnoise[i]['by']
    
    from operator import itemgetter
    outallnoise = sorted(allnoise.values(), key=itemgetter('id'))
    outallnoise.sort(compareNoisyExamples)
    
    output_dict = {}
    output_dict['allnoise'] = outallnoise
    output_dict['selection'] = {}
    return output_dict
    
def compareNoisyExamples(item1, item2):
    len1 = len(item1["by"])
    len2 = len(item2["by"])
    if len1 > len2: # reversed, want to have decreasing order 
        return -1
    elif len1 < len2: # reversed, want to have decreasing order 
        return 1
    else:
        return 0
    
def noiserank_select(postdata,input_dict, output_dict):
    try:    
        outselection = postdata['selected']
        data = input_dict['data']
        selection = [0]*len(data)
        for i in outselection:
            selection[int(i)] = 1
            outdata = data.select(selection, 1)
        output_dict['selection'] = outdata if outdata != None else None
    except KeyError:
        output_dict['selection'] = None
    
    #output_dict['selection'] = outselection if outselection != None else None
    return output_dict


# EVALUATION OF NOISE DETECTION PERFORMANCE
    
def add_class_noise(input_dict):
    import noiseAlgorithms4lib    
    output_dict = noiseAlgorithms4lib.insertNoise(input_dict)
    return output_dict
    
def aggr_results(input_dict):
    output_dict = {}
    output_dict['aggr_dict'] = { 'positives' : input_dict['pos_inds'], 'by_alg': input_dict['detected_inds']}
    return output_dict
    
def eval_batch(input_dict):
    alg_perfs = input_dict['perfs']
    beta = float(input_dict['beta'])
    performances = []    
    for exper in alg_perfs:
        noise = exper['positives']
        nds = exper['by_alg']
            
        performance = []
        for nd in nds:
            nd_alg = nd['name']
            det_noise = nd['inds']
            inboth = set(noise).intersection(set(det_noise))
            recall = len(inboth)*1.0/len(noise) if len(noise) > 0 else 0
            precision = len(inboth)*1.0/len(det_noise) if len(det_noise) > 0 else 0
            
            print beta, recall, precision
            if precision == 0 and recall == 0:
                fscore = 0
            else:
                fscore = (1+beta**2)*precision*recall/((beta**2)*precision + recall)
            performance.append({'name':nd_alg, 'recall': recall, 'precision' : precision, 'fscore' : fscore, 'fbeta': beta})
        
        performances.append(performance)
    
    output_dict = {}
    output_dict['perf_results'] = performances
    return output_dict

def eval_noise_detection(input_dict):
    noise = input_dict['noisy_inds']
    nds = input_dict['detected_noise']
        
    performance = []
    for nd in nds:
        nd_alg = nd['name']
        det_noise = nd['inds']
        inboth = set(noise).intersection(set(det_noise))
        recall = len(inboth)*1.0/len(noise) if len(noise) > 0 else 0
        precision = len(inboth)*1.0/len(det_noise) if len(det_noise) > 0 else 0
        beta = float(input_dict['f_beta'])
        print beta, recall, precision
        if precision == 0 and recall == 0:
            fscore = 0
        else:
            fscore = (1+beta**2)*precision*recall/((beta**2)*precision + recall)
        performance.append({'name':nd_alg, 'recall': recall, 'precision' : precision, 'fscore' : fscore, 'fbeta': beta})
    
    from operator import itemgetter
    output_dict = {}
    output_dict['nd_eval'] = sorted(performance, key=itemgetter('name'))  
    return output_dict
    
def avrg_std(input_dict):
    perf_results = input_dict['perf_results']
    stats = {}
    # Aggregate performance results
    n = len(perf_results)
    for i in range(n):
        for item in perf_results[i]:
            alg = item['name']
            if not stats.has_key(alg):
                stats[alg] = {}
                stats[alg]['precisions'] = [item['precision']]
                stats[alg]['recalls'] = [item['recall']]
                stats[alg]['fscores'] = [item['fscore']]
                stats[alg]['fbeta'] = item['fbeta']
            else:
                stats[alg]['precisions'].append(item['precision'])
                stats[alg]['recalls'].append(item['recall'])
                stats[alg]['fscores'].append(item['fscore'])
            
            # if last experiment: compute averages    
            if i == n-1:
                stats[alg]['avrg_pr'] = reduce(lambda x,y: x+y, stats[alg]['precisions'])/n
                stats[alg]['avrg_re'] = reduce(lambda x,y: x+y, stats[alg]['recalls'])/n
                stats[alg]['avrg_fs'] = reduce(lambda x,y: x+y, stats[alg]['fscores'])/n
        
    # Compute Standard Deviations
    import numpy
    avrgstdout = []
    print stats
    for alg, stat in stats.items():
        avrgstdout.append({'name': alg, 'precision': stat['avrg_pr'], 'recall': stat['avrg_re'],
                           'fscore' : stat['avrg_fs'],
                           'fbeta'  : stat['fbeta'],
                           'std_pr' : numpy.std(stat['precisions']),
                           'std_re' : numpy.std(stat['recalls']),
                           'std_fs' : numpy.std(stat['fscores']) })
                         
    from operator import itemgetter
    output_dict = {}
    output_dict['avrg_w_std'] = sorted(avrgstdout, key=itemgetter('name'))
    return output_dict

# VISUALIZATIONS
            
def pr_space(input_dict):
    return {}
    
def eval_bar_chart(input_dict):
    return {}
    
def eval_to_table(input_dict):
    return {}   
    
def data_table(input_dict):
    return {}

def data_info(input_dict):
    return {}

def sdmsegs(input_dict):
    return{}

def definition_sentences(input_dict):
    return {}

def term_candidates(input_dict):
    return {}

# FILE LOADING

def uci_to_odt(input_dict):
    from mothra.settings import FILES_FOLDER
    import orange
    output_dict = {}
    output_dict['data'] = orange.ExampleTable(FILES_FOLDER+"uci-datasets/"+input_dict['filename'])
    return output_dict
    
def odt_to_arff(input_dict):
    from noiseAlgorithms4lib import toARFFstring
    output_dict = {}
    f = toARFFstring(input_dict['odt'])
    output_dict['arff'] = f.getvalue()
    return output_dict  
             
# NLP tools

def merge_sentences(input_dict):
    """
    Merges the input sentences in XML according to the specified method.
    """
    method = input_dict['method']
    merged_sen, id_to_sent = set(), {}
    ids_list = []
    for sentsXML in input_dict['sentences']:
        sents = nlp.parse_def_sentences(sentsXML)
        ids = set(map(lambda x: x['id'], sents))
        ids_list.append(ids)
        # Save the map from id to sentence
        for sent in sents:
            id_to_sent[sent['id']] = sent
        if len(merged_sen) == 0:
            merged_sen = ids
        if method == 'union':
            merged_sen = merged_sen | ids
        elif method == 'intersection':
            merged_sen = merged_sen & ids
        elif method == 'intersection_two':
            for ids_alt in ids_list:
                merged_sen = merged_sen | (ids_alt & ids)
    return {'merged_sentences' : nlp.sentences_to_xml([id_to_sent[sid] for sid in merged_sen])}

def load_corpus(input_dict):
    '''
    Parses an input file and encodes it in base 64.
    '''
    import os.path
    import base64
    f = safeOpen(input_dict['file'])
    fname = os.path.basename(input_dict['file'])
    data = base64.b64encode(f.read())
    from services.webservice import WebService
    ws = WebService('http://bodysnatcher.ijs.si:8092/totale?wsdl', 600)
    response = ws.client.parseFile(fileName=fname, inFile=data)
    return {'corpus' : response['parsedFile']}

def kepner_tregoe(input_dict):
    output_dict = input_dict
    output_dict['model'] = None
    return output_dict

class KepnerTregoe:
    '''
    Kepner Tregoe model.
    '''
    def __init__(self, data, weights, smaller_is_better=None):
        self.data = data
        self.weights = weights
        self.smaller_is_better = smaller_is_better if smaller_is_better else set()
        self.name = 'Kepner-Tregoe'
    def __call__(self, weights=None):
        import Orange
        from Orange.feature import Type
        if weights == None:
            weights = self.weights
        # New augmented table
        norm_data = Orange.data.Table(self.data)
        newid = min(norm_data.domain.get_metas().keys()) - 1
        score_attr = Orange.feature.Continuous('score')
        norm_data.domain.add_meta(newid, score_attr)
        norm_data.add_meta_attribute(score_attr)
        # Normalize the attributes column-wise
        for att in norm_data.domain:
            if att.var_type == Type.Continuous:
                col = [ex[att] for ex in norm_data]
                col_norm = float(sum(col))
            for ex in norm_data:
                if att.var_type == Type.Continuous:
                    ex[att] = ex[att] / col_norm 
        # Use the inverse of an attr. value if smaller values should be treated as 'better'.
        inverse = lambda x, att: 1-x if att in self.smaller_is_better else x
        for ex in norm_data:
            score = sum([inverse(ex[att], att.name) * weights.get(att.name, 1) for att in norm_data.domain.features if att.var_type == Type.Continuous])
            ex['score'] = score
        return norm_data

def kepner_tregoe_finished(postdata, input_dict, output_dict):
    # Fetch the data and the weights from the form.
    data = input_dict['data']
    attributes = [att.name for att in data.domain.features]
    weights = {}
    widget_id = postdata['widget_id'][0]
    smaller_is_better = set()
    for att in attributes:
        weights[att]=int(postdata['weight'+str(widget_id)+str(att)][0])
        if postdata.has_key('smallerIsBetter'+str(widget_id)+str(att)):
            smaller_is_better.add(att)
    # Instantiate a KepnerTregoe model.
    kt = KepnerTregoe(data, weights, smaller_is_better=smaller_is_better)
    output_dict = {}
    output_dict['data'] = kt()
    output_dict['model'] = kt
    return output_dict

def sensitivity_analysis(input_dict):
    return input_dict

def ds_charts(input_dict):
    return input_dict

def string_to_file(input_dict):
    return {}

#------------------------------------------------------------------------------
# LATINO INTERFACE
#------------------------------------------------------------------------------
import latino
import logging

# Loading and saving adc files
def load_adc(input_dict):
    logging.info('__load_adc__')
    output_dict = {}
    output_dict['adc'] = latino.loadADC(input_dict['file'], input_dict['leadingLabels'])
    return output_dict
def load_adc_from_string(input_dict):
    logging.info('__load_adc_from_string__')
    output_dict = {}
    output_dict['adc'] = latino.loadADCFromString(input_dict['string'], input_dict['leadingLabels'])
    return output_dict
def get_strings(input_dict):
    logging.info('__get_strings__')
    output_dict = {}
    output_dict['strings'] = latino.getStrings(input_dict['adc'], input_dict['elementAnnotation'], input_dict['elementFeatureConditions'], input_dict['delimiter'], input_dict['includeDocId'])
    return output_dict
def extract_documents_features(input_dict):
    logging.info('__extract_documents_features__')
    output_dict = {}
    output_dict['strings'] = latino.ExtractDocumentsFeatures(input_dict['adc'], input_dict['featureName'])
    return output_dict
def add_documents_features(input_dict):
    logging.info('__add_documents_features__')
    output_dict = {}
    output_dict['adc'] = latino.AddDocumentsFeatures(input_dict['adc'], input_dict['featureValues'], input_dict['featureName'], input_dict['featureValuePrefix'])
    return output_dict
def split_documents_by_feature_value(input_dict):
    logging.info('__split_documents_by_feature_value__')
    output_dict = latino.SplitDocumentsByFeatureValue(input_dict['adc'], input_dict['featureCondition'])
    return output_dict
def mark_documents_with_sets(input_dict):
    logging.info('__mark_documents_with_sets__')
    output_dict = {}
    output_dict['adc'] = latino.MarkDocumentsWithSetFeature(input_dict['adc'], input_dict['featureName'], input_dict['featureValuePrefix'], \
        input_dict['numOfSets'], input_dict['random'], input_dict['useSeed'], input_dict['randomSeed'])
    return output_dict
def corpus_stataistics(input_dict):
    logging.info('__corpus_stataistics__')
    output_dict = latino.CorpusStatistics(input_dict['adc'])
    return output_dict

# ADC conversion
def adc2xml(input_dict):
    logging.info('__adc2xml__')
    output_dict = {}
    output_dict['string'] = latino.ADCtoXMLString(input_dict['adc'])
    return output_dict
def xml2adc(input_dict):
    logging.info('__xml2adc__')
    output_dict = {}
    output_dict['adc'] = latino.XMLStringtoADC(input_dict['string'])
    return output_dict

# Text tokenization
def create_english_me_sentence_detector(input_dict):
    logging.info('__create_english_me_sentence_detector__')
    output_dict = {}
    output_dict['tokenizer'] = latino.CreateEnglishMaximumEntropySentenceDetector(True)
    return output_dict
def create_english_me_tokenizer(input_dict):
    logging.info('__create_english_me_tokenizer__')
    output_dict = {}
    output_dict['tokenizer'] = latino.CreateEnglishMaximumEntropyTokenizer(True, input_dict['alphaNumericOptimization'])
    return output_dict
def create_unicode_tokenizer(input_dict):
    logging.info('__create_unicode_tokenizer__')
    output_dict = {}
    output_dict['tokenizer'] = latino.CreateUnicodeTokenizer(input_dict['filter'], input_dict['minTokenLen'])
    return output_dict
def create_simple_tokenizer(input_dict):
    logging.info('__create_simple_tokenizer__')
    output_dict = {}
    output_dict['tokenizer'] = latino.CreateSimpleTokenizer(input_dict['type'], input_dict['minTokenLen'])
    return output_dict
def create_regex_tokenizer(input_dict):
    logging.info('__create_regex_tokenizer__')
    output_dict = {}
    output_dict['tokenizer'] = latino.CreateRegexTokenizer(input_dict['tokenRegex'], input_dict['ignoreUnknownTokens'],
        'false', input_dict['regexOptionsIgnoreCase'], input_dict['regexOptionsMultiline'],
        input_dict['regexOptionsExplicitCapture'], "true", input_dict['regexOptionsSingleline'],
        input_dict['regexOptionsIgnorePatternWhitespace'], input_dict['regexOptionsRightToLeft'], input_dict['regexOptionsECMAScript'],
        input_dict['regexOptionsCultureInvariant'])
    return output_dict
def tokenize(input_dict):
    logging.info('__tokenize__')
    output_dict = {}
    output_dict['adc'] = latino.Tokenize(input_dict['adc'], input_dict['tokenizer'], input_dict['inputAnnotation'], input_dict['outputAnnotation'])
    return output_dict
def tokenize_string(input_dict):
    logging.info('__tokenize_string__')
    output_dict = {}
    output_dict['string'] = latino.TokenizeString(input_dict['string'], input_dict['tokenizer'])
    return output_dict
def tokenize_multiple(input_dict):
    logging.info('__tokenize_multiple__')
    output_dict = {}
    output_dict['adc'] = latino.TokenizeMultiple(input_dict['adc'], input_dict['tokenizer'], input_dict['inputAnnotation'], input_dict['outputAnnotation'])
    return output_dict

# POS Tagging
def create_english_me_postagger(input_dict):
    logging.info('__create_english_me_postagger__')
    output_dict = {}
    output_dict['posTagger'] = latino.CreateEnglishMaximumEntropyPosTagger(input_dict['beamSize'], True)
    return output_dict
def pos_tag(input_dict):
    logging.info('__pos_tag__')
    output_dict = {}
    output_dict['adc'] = latino.PosTag(input_dict['adc'], input_dict['posTagger'], input_dict['groupAnnotation'], input_dict['elementAnnotation'], input_dict['outputFeature'])
    return output_dict
def pos_tag_string(input_dict):
    logging.info('__pos_tag_string__')
    output_dict = {}
    output_dict['string'] = latino.PosTagString(input_dict['string'], input_dict['posTagger'], input_dict['outputFeature'])
    return output_dict

# Tagging
def create_lemmasharp_lemmatizer(input_dict):
    logging.info('__create_lemmasharp_lemmatizer__')
    output_dict = {}
    output_dict['tagger'] = latino.ConstructLemmaSharpLemmatizer(input_dict['language'])
    return output_dict
def create_snowball_stemmer(input_dict):
    logging.info('__create_snowball_stemmer__')
    output_dict = {}
    output_dict['tagger'] = latino.ConstructSnowballStemmer(input_dict['language'])
    return output_dict
def create_stopwords_tagger(input_dict):
    logging.info('__create_stopwords_tagger__')
    output_dict = {}
    output_dict['tagger'] = latino.ConstructStopWordsTagger(input_dict['stopWords'], input_dict['ignoreCase'])
    return output_dict
def create_condition_tagger(input_dict):
    logging.info('__create_condition_tagger__')
    output_dict = {}
    output_dict['tagger'] = latino.ConstructConditionTagger(input_dict['featureCondition'], input_dict['outputFeatureValue'], input_dict['elementsTextToFeatureValue'])
    return output_dict
def tag_string(input_dict):
    logging.info('__tag_string__')
    output_dict = {}
    output_dict['string'] = latino.TagString(input_dict['string'], input_dict['tagger'], input_dict['outputFeature'])
    return output_dict
def tag_adc(input_dict):
    logging.info('__tag_adc__')
    output_dict = {}
    output_dict['adc'] = latino.TagADC(input_dict['adc'], input_dict['tagger'], input_dict['elementAnnotation'], input_dict['outputFeature'])
    return output_dict
def tag_adc_multiple(input_dict):
    logging.info('__tag_adc_multiple__')
    output_dict = {}
    output_dict['adc'] = latino.TagADCMultiple(input_dict['adc'], input_dict['tagger'], input_dict['elementAnnotation'], input_dict['outputFeature'])
    return output_dict

# Tagging helper functions
def get_stopwords(input_dict):
    logging.info('__get_stopwords__')
    output_dict = {}
    output_dict['strings'] = latino.GetStopWords(input_dict['language'])
    return output_dict

# Bow creation and manipulation
def create_bow_space_from_texts(input_dict):
    logging.info('__create_bow_space_from_texts__')
    output_dict = latino.ConstructBowSpaceFromTexts(input_dict['documents'], input_dict['tokenizer'], input_dict['stemmer'], input_dict['stopwords'], \
        input_dict['maxNGramLen'], input_dict['minWordFreq'], input_dict['wordWeightType'], input_dict['cutLowWeightsPerc'], input_dict['normalizeVectors'])
    return output_dict
def create_bow_space_from_adc(input_dict):
    logging.info('__create_bow_space_from_adc__')
    output_dict = latino.ConstructBowSpaceFromADC(input_dict['adc'], input_dict['tokenId'], input_dict['stemId'], input_dict['stopwordId'], input_dict['labelId'],\
        input_dict['maxNGramLen'], input_dict['minWordFreq'], input_dict['wordWeightType'], input_dict['cutLowWeightsPerc'], input_dict['normalizeVectors'])
    return output_dict

def bow_get_vocabulary(input_dict):
    logging.info('__bow_get_vocabulary__')
    output_dict = {}
    output_dict['vocabulary'] = latino.GetVocabulary(input_dict['bow'], input_dict['startIndex'], input_dict['maxWords'])
    return output_dict
def bow_get_document_vectors(input_dict):
    logging.info('__bow_get_document_vectors__')
    output_dict = {}
    output_dict['ds'] = latino.GetDocumentVectors(input_dict['bow'])
    return output_dict
def bow_process_new_documents(input_dict):
    logging.info('__bow_process_new_documents__')
    output_dict = {}
    output_dict['ds'] = latino.ProcessNewDocuments(input_dict['bow'], input_dict['obj'])
    return output_dict

# Document vectors manipulation
def get_labeled_dataset(input_dict):
    logging.info('__get_labeled_dataset__')
    output_dict = {}
    output_dict['ds'] = latino.AddLabelsToDocumentVectors(input_dict['ds'], input_dict['labels'])
    return output_dict
def get_unlabeled_dataset(input_dict):
    logging.info('__get_unlabeled_dataset__')
    output_dict = {}
    output_dict['ds'] = latino.RemoveDocumentVectorsLabels(input_dict['ds'])
    return output_dict
def extract_dataset_labels(input_dict):
    logging.info('__extract_dataset_labels__')
    output_dict = {}
    output_dict['labels'] = latino.ExtractDatasetLabels(input_dict['ds'])
    return output_dict
def dataset_split_simple(input_dict):
    logging.info('__dataset_split_simple__')
    return latino.DatasetSplitSimple(input_dict)

def calculate_similarity_matrix(input_dict):
    logging.info('__calculate_similarity_matrix__')
    output_dict = {}
    output_dict["smx"] = latino.CalculateSimilarityMatrix(input_dict['dv1'], input_dict['dv2'], input_dict['thresh'], input_dict['fullMatrix'])
    return output_dict
def matrix_to_table(input_dict):
    logging.info('__matrix_to_table__')
    output_dict = {}
    output_dict["tbl"] = latino.SparseMatrixToTable(input_dict['smx'])
    return output_dict

# Clustering
def create_kmeans_clusterer(input_dict):
    logging.info('__create_kmeans_clusterer__')
    output_dict = {}
    output_dict['clusterer'] = latino.ConstructKMeansClusterer(input_dict['k'], input_dict['centroidType'], input_dict['similarityModel'], input_dict['randomSeed'], input_dict['eps'], input_dict['trials'])
    return output_dict
def create_kmeans_fast_clusterer(input_dict):
    logging.info('__create_kmeans_fast_clusterer__')
    output_dict = {}
    output_dict['clusterer'] = latino.ConstructKMeansFastClusterer(input_dict['k'], input_dict['randomSeed'], input_dict['eps'], input_dict['trials'])
    return output_dict
def create_hier_bisect_clusterer(input_dict):
    logging.info('__create_hier_bisect_clusterer__')
    output_dict = {}
    output_dict['clusterer'] = latino.ConstructHierarchicalBisectingClusterer(input_dict['minQuality'])
    return output_dict

def cluster_document_vectors(input_dict):
    logging.info('__cluster_document_vectors__')
    output_dict = {}
    output_dict['clustRes'] = latino.ClusterDocumentVectors(input_dict['clusterer'], input_dict['ds'])
    return output_dict
def clustering_results_info(input_dict):
    logging.info('__clustering_results_info__')
    dict = latino.ClusteringResultsInfo(input_dict['clustRes'])
    output_dict = {'docLabels':dict['docLabels'],'clustTree':dict['clustTree']}
    return output_dict

# Classification
def construct_centroid_classifier(input_dict):
    logging.info('__construct_centroid_classifier__')
    return latino.ConstructCentroidClassifier(input_dict)
def construct_naivebayes_classifier(input_dict):
    logging.info('__construct_naivebayes_classifier__')
    return latino.ConstructNaiveBayesClassifier(input_dict)
def construct_svm_binary_classifier(input_dict):
    logging.info('__construct_svm_binary_classifier__')
    return latino.ConstructSvmBinaryClassifier(input_dict)
def construct_svm_multiclass_fast(input_dict):
    logging.info('__construct_svm_multiclass_fast__')
    return latino.ConstructSvmMulticlassFast(input_dict)
def construct_majority_classifier(input_dict):
    logging.info('__construct_majority_classifier__')
    return latino.ConstructMajorityClassifier(input_dict)
def construct_maximum_entropy_classifier(input_dict):
    logging.info('__construct_maximum_entropy_classifier__')
    return latino.ConstructMaximumEntropyClassifier(input_dict)
def construct_maximum_entropy_classifier_fast(input_dict):
    logging.info('__construct_maximum_entropy_classifier_fast__')
    return latino.ConstructMaximumEntropyClassifierFast(input_dict)
def construct_knn_classifier(input_dict):
    logging.info('__construct_knn_classifier__')
    return latino.ConstructKnnClassifier(input_dict)
def construct_knn_classifier_fast(input_dict):
    logging.info('__construct_knn_classifier_fast__')
    return latino.ConstructKnnClassifierFast(input_dict)

def train_classifier(input_dict):
    logging.info('__train_classifier__')
    output_dict = {}
    output_dict['classifier'] = latino.TrainClassifier(input_dict['classifier'], input_dict['ds'])
    return output_dict

def predict_classification(input_dict):
    logging.info('__predict_classification__')
    output_dict = latino.PredictClassification(input_dict['classifier'], input_dict['ds'])
    return output_dict

def prediction_to_label(input_dict):
    logging.info('__prediction_to_label__')
    output_dict = {}
    output_dict['labels'] = latino.PredictionToLabel(input_dict['predictions'])
    return output_dict
def prediction_info(input_dict):
    logging.info('__prediction_info__')
    output_dict = latino.PredictionInfo(input_dict['predictions'])
    return output_dict

def cross_validation(input_dict):
    logging.info('__cross_validation__')
    output_dict = {}
    output_dict['obj'] = latino.CrossValidation(input_dict['csf'], input_dict['ds'], input_dict['numOfSets'], input_dict['random'], input_dict['useSeed'], input_dict['randomSeed'])
    return output_dict
def cross_validation_predef_splits(input_dict):
    logging.info('__cross_validation_predef_splits__')
    output_dict = {}
    output_dict['obj'] = latino.CrossValidationPredefSplits(input_dict['csf'], input_dict['ds'], input_dict['sets'])
    return output_dict

#------------------------------------------------------------------------------
# VISUALISATIONS
#------------------------------------------------------------------------------
def show_adc(input_dict):
    logging.info('__show_adc__FUNCT__')
    if input_dict["adc"] == None:
        raise Exception("Input ADC is required for displaying Anotated Document Corpus!")
    return {}
def advanced_object_viewer(input_dict):
    logging.info('__advanced_object_viewer__FUNCT__')
    return {}
def show_table(input_dict):
    logging.info('__show_table__FUNCT__')
    return {}

#------------------------------------------------------------------------------
# SUPPLEMENTARY FUNCTIONS
#------------------------------------------------------------------------------
def object_splitter(input_dict):
    logging.info('__get_stopwords__')
    output_dict = {}
    obj = input_dict['object']
    output_dict['object'] = eval("obj"+input_dict['attribute'])
    return output_dict
def python_snippet(input_dict):
    logging.info('__python_snippet__')
    output_dict = {}
    input = input_dict['in']
    for (i, val) in enumerate(input):
        vars()["in"+str(i+1)] = val
    out1 = None
    exec(input_dict['pycode'])
    output_dict['out'] = out1
    return output_dict
def csharp_snippet(input_dict):
    logging.info('__csharp_snippet__')
    output_dict = latino.RunCSharpSnippet(input_dict['in'], input_dict['snippetCode'], input_dict['aditionalReferences'], input_dict['usings'])
    return output_dict
def flatten_to_string_array(input_dict):
    logging.info('__flatten_to_string_array__')
    output_dict = {}
    obj = input_dict['object']
    output_dict['object'] = latino.FlattenObjectToStringArray(obj)
    return output_dict
def create_range(input_dict):
    rng = range(latino.ToInt(input_dict['start']), latino.ToInt(input_dict['stop']), latino.ToInt(input_dict['step']))
    return {'range':rng }
def compare_lists(input_dict):
    l1 = input_dict['list1']
    l2 = input_dict['list2']
    l = min(len(l1),len(l2))
    cntEq = 0;
    for i in range(0,l):
        if l1[i]==l2[i]:
            cntEq += 1
    return {
        'accuracy':(0.0+cntEq)/l,
        'statistics':{
            'elements':l,
            'equal':cntEq,
            'different':l-cntEq,
            'accuracy':(0.0+cntEq)/l,
            'error':(0.0+l-cntEq)/l,
        }
    }



