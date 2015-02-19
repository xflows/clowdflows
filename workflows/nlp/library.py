import nlp
import os.path
import base64
from services.webservice import WebService
from workflows.security import safeOpen
import requests
import json
import re
import itertools

webservices_url = "http://vihar.ijs.si:8104"

def merge_sentences(input_dict):
    """
    Merges the input sentences in XML according to the specified method.
    """
    method = input_dict['method']
    merged_sen, id_to_sent = set(), {}
    ids_list = []
    for i, sentsXML in enumerate(input_dict['sentences']):
        sents = nlp.parse_def_sentences(sentsXML)
        ids = set(map(lambda x: x['id'], sents))
        ids_list.append(ids)
        # Save the map from id to sentence
        for sent in sents:
            id_to_sent[sent['id']] = sent
        if i == 0 and method != 'intersection_two':
            merged_sen = ids
        if method == 'union':
            merged_sen = merged_sen | ids
        elif method == 'intersection':
            merged_sen = merged_sen & ids
        elif method == 'intersection_two':
            # Skip the current set of sentences
            # and intersect it with the others.
            for ids_alt in ids_list[:i] + ids_list[i+1:]:
                # As long as (at least) two sets agree with a sentence it 
                # will be in the resulting set.
                merged_sen = merged_sen | (ids_alt & ids)
    return {'merged_sentences': nlp.sentences_to_xml([id_to_sent[sid] for sid in merged_sen])}


def load_corpus(input_dict):
    '''
    Parses an input file and encodes it in base 64.
    '''
    f = safeOpen(input_dict['file'])
    fname = os.path.basename(input_dict['file'])
    wsdl = input_dict.get('wsdl', 'http://vihar.ijs.si:8095/totale?wsdl')
    data = base64.b64encode(f.read())
    ws = WebService(wsdl, 60000)
    response = ws.client.parseFile(fileName=fname, inFile=data)
    return {'corpus': response['parsedFile']}

def load_corpus2(input_dict):
    '''
    Parses an input file and encodes it in base 64.
    '''

    if input_dict[u"text"] == "":
        f = safeOpen(input_dict['file'])
        fname = os.path.basename(input_dict['file'])
        data = base64.b64encode(f.read())
    else:
        fname = "input_string.txt"
        data = base64.b64encode(input_dict[u"text"].strip())
    
    #define web service
    webservice_url = webservices_url + "/parseFile"
    params = {"filename": fname, "text": data} #set params
    
    #call web service
    resp = requests.post(webservice_url, params=params)
    content = json.loads(resp.content)[u'parseFileResponse'][u'parseFileResult']
    
    if content[u"error"] != "":
        raise Exception(content[u"error"])
    else:
        return {'corpus': content[u"resp"]}

def load_tagged_corpus(input_dict):
    """
    Loads TEI file, which is output of totrtale
    """
    f = safeOpen(input_dict['file'])
    #fname = os.path.basename(input_dict['file'])
    #subprocess.call(["java -jar jing.jar tei_imp.rng " + fname + " >" + "out.txt"],shell=True)
    data = f.read()
    return {'annotations': data}

def nlp_totrtale2(input_dict):
    '''
    Calls the totrtale web service.
    '''
    corpus = input_dict['corpus']
    lang = input_dict['lang']
    xml = input_dict['xml']
    postprocess = input_dict['postprocess']
    bohoricica = input_dict['bohoricica']
    antique = input_dict['antique']

    #define web service
    webservice_url = webservices_url + "/runToTrTaLe"
    params = {"text":corpus, "language": lang, "postProcessing":postprocess, "bohoricica":bohoricica, "antique": antique, "outputAsXML":xml}

    import time
    start = time.time()
    response = requests.post(webservice_url, params = params)

    content = json.loads(response.content)
    if u'runToTrTaLeResponse' in content:
        content = content[u'runToTrTaLeResponse'][u'runToTrTaLeResult']
    else:
        content = content[u"error"]
    end = time.time()
    print "ToTrTale execution time was ", end - start
    return {'annotations': content[u'resp']}


def nlp_totrtale(input_dict):
    '''
    Calls the totrtale web service.
    '''
    corpus = input_dict['corpus']
    lang = input_dict['lang']
    wsdl = input_dict.get('wsdl', 'http://vihar.ijs.si:8095/totale?wsdl')
    xml = input_dict['xml'] == 'true'
    postprocess = input_dict['postprocess'] == 'true'
    bohoricica = input_dict['bohoricica'] == 'true'
    antique = input_dict['antique'] == 'true'
    print lang, wsdl, xml, postprocess, bohoricica, antique
    ws = WebService(wsdl, 60000)
    response = ws.client.runTotale(inFile=corpus, language=lang,
                                   postProcessing=postprocess,
                                   bohoricica=bohoricica,
                                   antiqueSlovenian=antique,
                                   outputAsXML=xml)
    errors = response['error']
    if errors:
        # todo report this as warning
        print errors
    return {'annotations': response['annotatedFile']}


def nlp_term_extraction(input_dict):
    '''
    Term extraction from totrtale annotations.
    '''
    annotations = input_dict['annotations']
    lang = input_dict['lang']
    wsdl = input_dict.get('wsdl', 'http://vihar.ijs.si:8095/totale?wsdl')

    if '<TEI xmlns="http://www.tei-c.org/ns/1.0">' in annotations:
        annotations = XMLtoTEI(annotations)

    ws = WebService(wsdl, 60000)
    response = ws.client.TermExtraction(corpus=annotations, lang=lang,
                                        threshold=0)
    return {'candidates': response['candidates']}


def nlp_def_extraction_patterns(input_dict):
    '''
    Definition extraction using pre-defined patterns.
    '''
    annotations = input_dict['annotations']
    lang = input_dict['lang']
    wsdl = input_dict.get('wsdl', 'http://vihar.ijs.si:8099')

    if '<TEI xmlns="http://www.tei-c.org/ns/1.0">' in annotations:
        annotations = XMLtoTEI(annotations)

    ws = WebService(wsdl, 60000)
    pattern = input_dict['pattern']
    response = ws.client.GlossaryExtractionByPatterns(corpus=annotations,
                                                      lang=lang, pattern=pattern)
    return {'sentences': response['candidates']}


def nlp_def_extraction_terms(input_dict):
    '''
    Definition extraction using terms.
    '''
    annotations = input_dict['annotations']
    term_candidates = input_dict['term_candidates']
    lang = input_dict['lang']
    wsdl = input_dict.get('wsdl', 'http://vihar.ijs.si:8099')
    terms_per_sentence = input_dict['terms_per_sentence']
    nominatives = input_dict['nominatives']
    threshold = input_dict['threshold']
    verb_two_terms = input_dict['verb_two_terms']
    multiword_term = input_dict['multiword_term']
    num_multiterms = input_dict['num_multiterms']
    term_beginning = input_dict['term_beginning']

    if '<TEI xmlns="http://www.tei-c.org/ns/1.0">' in annotations:
        annotations = XMLtoTEI(annotations)
    
    ws = WebService(wsdl, 60000)
    response = ws.client.GlossaryExtractionByTerms(corpus=annotations,
        candidates=term_candidates, lang=lang, nominatives=nominatives,
        termsPerSent=terms_per_sentence, select=threshold, 
        verb_two_terms=verb_two_terms, multiword_term=multiword_term,
        num_multiterms=num_multiterms, term_beginning=term_beginning)
    return {'sentences': response['candidates']}


def nlp_def_extraction_wnet(input_dict):
    '''
    Definition extraction using WordNet.
    '''
    annotations = input_dict['annotations']
    lang = input_dict['lang']
    wsdl = input_dict.get('wsdl', 'http://vihar.ijs.si:8099')
    
    if '<TEI xmlns="http://www.tei-c.org/ns/1.0">' in annotations:
        annotations = XMLtoTEI(annotations)

    ws = WebService(wsdl, 60000)
    response = ws.client.GlossaryExtractionByWnet(corpus=annotations, lang=lang)
    return {'sentences': response['candidates']}


def XMLtoTEI(text):    
    mask1 = ["\tTOK\t", "\t", "\t\n"]
    pattern1 = "<w lemma=\"(?P<lemma>.*?)\" ana=\"(?P<ana>.*?)\">(?P<value>.*?)</w>"
    pattern2 = "<title>(.*?)</title>"
    pattern3 = "<pc>(.*?)</pc>"
    newText=[]
    for l in text.splitlines():
        if "<w" in l:
            match = [m.group("value", "lemma", "ana") for m in re.finditer(pattern1, l)][0]
            newText.append(''.join(itertools.chain.from_iterable(zip(match, mask1))).decode("utf8"))
        elif "</s>" in l:
            newText.append("\t\t<S/>\t\n")
        elif "<pc>" in l:
            value = re.findall(pattern3, l)[0]
            if value == ".":
                newText.append(value+"\t\tPUN_TERM\t\n")
            else:
                newText.append(value+"\t\tPUN\t\n")
        elif "<title>" in l:
            title = re.findall(pattern2, l)[0]
            newText.append("<TEXT title=" + title + ">\t\n")
        elif "</body>" in l:
            newText.append("</TEXT>\t\n")
    return "".join(newText)