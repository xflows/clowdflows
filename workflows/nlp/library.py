import nlp
import os.path
import base64
from services.webservice import WebService
from workflows.security import safeOpen
from requests import post
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
    resp = post(webservice_url, params=params)
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

def totrtale_request(params):
    webservice_url = webservices_url + "/runToTrTaLe"
    return post(webservice_url, params=params)

def nlp_totrtale2(input_dict, widget):
    '''
    Calls the totrtale web service.
    '''
    import multiprocessing
    from xml.dom.minidom import parseString
    import time
    import math
    import copy

    progress_accumulator = 0
    widget.progress= progress_accumulator
    widget.save()

    processes = 4
    DOCUMENTS_SIZE = 3 * int(1e6) #Document size (MB) per process
    SINGLE_DOC_SIZE = 1 * int(1e6)
    
    corpus = parseString(input_dict['corpus'])
    
    language = input_dict['lang'], 
    postprocess = input_dict['postprocess'] == "true"
    bohoricica = input_dict['bohoricica'] == "true"
    antique = input_dict['antique'] == "true" 
    xml = input_dict['xml'] == "true"

    params = {"language": language, 
            "postprocess": postprocess, 
            "bohoricica":bohoricica, 
            "antique": antique, 
            "xml":xml}
             
    tei_corpus = corpus.getElementsByTagName('teiCorpus')
    if tei_corpus:
        tei_head = '<?xml version="1.0" encoding="utf-8"?>\n' + \
                   '<teiCorpus xmlns="http://www.tei-c.org/ns/1.0">\n'
        tei_header = corpus.getElementsByTagName('teiHeader')[0].toxml() + "\n"
        tei_tail = '</teiCorpus>'

    pool = multiprocessing.Pool(processes=processes)
    documents = corpus.getElementsByTagName('TEI')
    documents_size, document_num, process_num = 0, 0, 1
    #titles = []

    results, docs, single_docs = [], [], []
    for i, document in enumerate(documents):
        doc_len = len(document.getElementsByTagName('body')[0].getElementsByTagName('p')[0].childNodes[0].nodeValue)
        doc_title = document.getElementsByTagName('title')[0].firstChild.nodeValue
        #titles.append(doc_title)
        print doc_title
        if doc_len > SINGLE_DOC_SIZE:
            
            predhead = '<TEI xmlns="http://www.tei-c.org/ns/1.0">\n'
            title = '<title>' + doc_title + '</title>\n'
            head = '<text>\n<body>\n<p>\n'
            header = document.getElementsByTagName('teiHeader')[0].toxml() + "\n"
            tail = '\n</p>\n</body>\n</text>\n</TEI>'
            

            document_text = document.getElementsByTagName('body')[0].getElementsByTagName('p')[0].childNodes[0].nodeValue.strip().replace("&","&amp;").replace("<","&lt;").replace(">","&gt;").replace("\"","&quot;")

            prev_j, curr_j  = 0, SINGLE_DOC_SIZE
            while (curr_j+2) < len(document_text):
                while (curr_j+2) < len(document_text) and document_text[curr_j:curr_j+2] != ". ":
                    curr_j+=1
                sub_params = copy.deepcopy(params)
                if prev_j == 0:
                    sub_params["text"] = predhead +title + head + document_text[prev_j: curr_j+2] +tail
                else:
                    sub_params["text"] = predhead + head + document_text[prev_j: curr_j+2] + tail

                results.append(pool.apply_async(totrtale_request, args=[sub_params]))
                if prev_j == 0:
                    single_docs.append(0)
                else:
                    single_docs.append(1)
                prev_j = curr_j+2
                curr_j += SINGLE_DOC_SIZE
                document_num+=1
                process_num += 1
                
                if curr_j > doc_len:
                    sub_params = copy.deepcopy(params)
                    sub_params["text"] = predhead + head + document_text[prev_j:] + tail
                    results.append(pool.apply_async(totrtale_request, args=[sub_params]))
                    document_num += 1
                    process_num += 1
                    single_docs.append(2)
            print "document was split",doc_title, len(single_docs)
        else:
            docs.append(document.toxml())
            document_num+=1
            documents_size += doc_len
            
            if documents_size > DOCUMENTS_SIZE or (document_num) % 10==0 or i == len(documents)-1:
                #print "Log:",process_num, "process added to queue with", document_num, "documents" 
                documents_size = 0
                document_num = 0
                sub_params = copy.deepcopy(params)
                sub_params["text"] = "\n".join(docs)
                print "whole document was added", len(docs)
                results.append(pool.apply_async(totrtale_request, args=[sub_params]))
                process_num += 1
                docs = []
                single_docs.append(-1)
    pool.close()

    response = ["" for i in results]
    progress = [True]
    
    while any(progress):
        time.sleep(1)
        progress = [not result.ready() for result in results]
        print progress
        for i, prog in enumerate(progress):
            if not prog and response[i] == "":
                try:
                    resp=json.loads(results[i].get().content)[u'runToTrTaLeResponse'][u'runToTrTaLeResult']
                except Exception as e:
                    raise Exception("There was a problem processing your file.")

                if resp["error"] != "":
                    progress = [False]
                    raise Exception(resp["error"])
                if xml:
                    if single_docs[i] == 0:
                        print "remove back", i
                        pos1 = resp["resp"].find("<s>")
                        pos2 = resp["resp"].find("</p>")
                        response[i] = predhead + header + head + resp["resp"][pos1:pos2]    
                    elif single_docs[i] == 2:
                        print "remove front", i
                        pos1 = resp["resp"].find("<s>")
                        response[i] = resp["resp"][pos1:]
                    elif single_docs[i] == 1:
                        print "remove both", i
                        pos1 = resp["resp"].find("<s>")
                        pos2 = resp["resp"].find("</p>")
                        response[i] = resp["resp"][pos1:pos2]
                    else:
                        print "nothing to remove"
                        response[i] = resp["resp"]
                else:
                    if single_docs[i] in [0,1]:
                        #print "remove back", i, single_docs[i]
                        #pos1 = resp["resp"].find("<p>")
                        pos2 = resp["resp"].find("</TEXT>")
                        response[i] = resp["resp"][:pos2]    
                    else:
                        print "nothing to remove"
                        response[i] = resp["resp"]

                progress_accumulator += 1/float(len(results))*100
                print progress_accumulator
                widget.progress = math.floor(progress_accumulator)

                widget.save()
    pool.join()
    
    if not any(progress):
        widget.progress=100
        widget.save()
        response = "".join(response)

        if tei_corpus and xml:
            response = tei_head + tei_header + response + tei_tail
        return {'annotations': response}

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
        annotations = TEItoTab(annotations)

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
        annotations = TEItoTab(annotations)

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
        annotations = TEItoTab(annotations)
    
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
        annotations = TEItoTab(annotations)

    ws = WebService(wsdl, 60000)
    response = ws.client.GlossaryExtractionByWnet(corpus=annotations, lang=lang)
    return {'sentences': response['candidates']}


def TEItoTab(text):    
    mask1 = ["\tTOK\t", "\t", "\t\n"]
    pattern1 = "<w lemma=\"(?P<lemma>.*?)\" ana=\"(?P<ana>.*?)\">(?P<value>.*?)</w>"
    pattern2 = "<title>(.*?)</title>"
    pattern3 = "<pc>(.*?)</pc>"
    
    pattern4 = "(.*?)\t(TOK)\t(.*?)\t(Y)"
    pattern5 = "(.*?)\t(TOK)\t(.*?)\t(Mdo|Mdc)"
    newText=[]
    for l in text.splitlines():
        if "<w" in l:
            match = [m.group("value", "lemma", "ana") for m in re.finditer(pattern1, l)][0]
            l = ''.join(itertools.chain.from_iterable(zip(match, mask1)))
            if len(l) < 100:
                value = re.findall(pattern4, l)
                if len(value) > 0:
                    l = "\t".join(value[0]).replace("TOK", "TOK_ABBR") + "\t\n"

                value = re.findall(pattern5, l)
                if len(value) > 0:
                    l = "\t".join(value[0]).replace("TOK", "TOK_DIG") + "\t\n"
            newText.append(l)
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