import nlp
import os
import base64
from services.webservice import WebService
from workflows.security import safeOpen
from requests import post
import json
import re
import itertools
import subprocess

webservices_totrtale_url = "http://172.20.0.154/totrtale"
webservice_def_ex_url = "http://172.20.0.154/definition"

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

def merge_sentences2(input_dict):
    """
    Merges the input sentences in XML according to the specified method.
    """
    method = input_dict['method']
    merged_sen, id_to_sent = set(), {}
    ids_list = []
    for i, sentsXML in enumerate(input_dict['sentences']):
        sents = nlp.parse_def_sentences2(sentsXML)
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
    return {'merged_sentences': nlp.sentences_to_xml2([id_to_sent[sid] for sid in merged_sen])}


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
    use_text = input_dict["use_text"] == "true"

    if use_text: #checkbox is checked
        fname = "input_string.txt"
        text = input_dict[u"text"].strip()
        if len(text) == 0:
            raise Exception("Please input text or uncheck the Use text checkbox.")
        data = base64.b64encode(text)
    else: #checkbox is not checked
        f = safeOpen(input_dict['file'])
        fname = os.path.basename(input_dict['file'])
        data = base64.b64encode(f.read())
    
    #define web service
    webservice_url = webservices_totrtale_url + "/parseFile"
    params = {"filename": fname, "text": data} #set params
    
    #call web service
    #print webservice_url
    resp = post(webservice_url, data=params)
    #print resp.content
    content = json.loads(resp.content)[u'parseFileResponse'][u'parseFileResult']
    """
    if content[u"error"] != "":
        raise Exception(content[u"error"])
    else:
    """
    return {'corpus': content[u"resp"]}

def parse_tei(path, lemma_name = "lemma", pos_name = "ana", word_tag = "w", sentence_tag = "s"):
    """
    Helper function for load tagged corpus. Function parses TEI format.
    """
    from xml.dom import minidom

    fname = os.path.basename(path)
    xmldoc = minidom.parse(path)
    sentences = xmldoc.getElementsByTagName(sentence_tag)

    tab_separated_output = []
    head = "<TEXT title="+fname+">\t\n"
    foot = "</TEXT>\t\n"
    tab_separated_output.append(head)

    sentence_id = 0
    for sentece in sentences:
        line = "\t<S id=\"0_" +str(sentence_id) + "\">\t\n" 
        tab_separated_output.append(line)
        for s in sentece.getElementsByTagName(word_tag):
            line = s.childNodes[0].nodeValue + "\tTOK\t" + s.attributes[lemma_name].value + "\t" + s.attributes[pos_name].value + "\t\n"
            tab_separated_output.append(line)
        line = "\t</S>\t\n"
        tab_separated_output.append(line)
        sentence_id +=1
    tab_separated_output.append(foot)
    return  "".join(tab_separated_output).encode("utf8", "ignore")

def parse_tab_separated(path, word_index, token_index, lemma_index, pos_index, start_tag, end_tag, separator):
    """
    Helper function for load tagged corpus. Function parses tab separated format.
    """
    
    fname = os.path.basename(path)
    f = safeOpen(path)

    data = []
    head = "<TEXT title="+fname+">\t\n"
    foot = "</TEXT>\t\n"
    data.append(head)

    sentence_counter = 0
    for line in f:
        splitted_line = re.split(separator, line.strip())
        if len(splitted_line) >= 4:
            new_line = splitted_line[word_index] + "\t" + splitted_line[token_index] + "\t" + splitted_line[lemma_index] + "\t" + splitted_line[pos_index] + "\t\n"
            data.append(new_line)
        else:
            added = False
            for el in splitted_line:
                if re.match(start_tag, el.strip()):
                    data.append("\t<S id=\"0_" + str(sentence_counter)+"\">\t\n")
                    added = True
                    break
                elif re.match(end_tag, el.strip()):
                    data.append("\t</S>\t\n")
                    sentence_counter+=1
                    added = True
                    break
            if not added:
                data.append("\t".join(splitted_line + ["\t\n"]))
    data.append(foot)
    return "".join(data)

def load_tagged_corpus(input_dict):
    """
    Loads a file in TEI or XML format.
    """
    data = ""
    
    if input_dict["input_format"] == "tab_format":
        try:
            word_index = int(input_dict["word_index"]) - 1
            lemma_index = int(input_dict["lemma_index"]) - 1
            token_index = int(input_dict["token_index"]) - 1
            pos_index = int(input_dict["pos_index"]) - 1
        except ValueError:
            raise Exception("Please specify a number in index fields.")

        start_tag = input_dict["start_tag"]
        end_tag = input_dict["end_tag"]
        separator = input_dict["separator"]

        if len(start_tag) < 1 or len(end_tag) < 1 or len(separator) < 1:
            raise Exception("Please review start, end tag and separator parameters.")
        
        if word_index+1 == 1 and token_index+1 == 2 and lemma_index+1 == 3 and pos_index+1 == 4 and start_tag == u'<S>' and end_tag == '</S>':
            f = safeOpen(input_dict['file'])
            data = f.read()
        else:
            if len(set([word_index, lemma_index, token_index, pos_index])) != 4:
                raise Exception("Field indices should be distinct.")
            data = parse_tab_separated(input_dict['file'], word_index=word_index, token_index=token_index, lemma_index=lemma_index, pos_index=pos_index, start_tag=start_tag, end_tag=end_tag, separator=separator)

    else:
        lemma_name = input_dict["lemma_name"]
        pos_name = input_dict["pos_name"]
        sentence_tag = input_dict["sentence_tag"]
        word_tag = input_dict["word_tag"]

        if len(lemma_name) < 1 or len(pos_name) < 1 or len(sentence_tag) < 1 or len(word_tag) < 1:
            raise Exception("Please review parameters for TEI format.")

        data = parse_tei(input_dict['file'], lemma_name = lemma_name, pos_name = pos_name, word_tag = word_tag, sentence_tag = sentence_tag)

    return {'annotations': data}

def totrtale_request(params):
    webservice_url = webservices_totrtale_url + "/runToTrTaLe"
    return post(webservice_url, data=params)

def nlp_totrtale2(input_dict, widget):
    '''
    Calls the totrtale web service.

    Function splits huge documents in smaller pieces and sends them separatly to totrtale webservice. If there is multiple smaller documents, this functions groups them and sends them together.
    '''
    import multiprocessing
    from xml.dom.minidom import parseString
    import time
    import math
    import copy

    progress_accumulator = 0 #progress for progress bar
    widget.progress= progress_accumulator 
    widget.save()

    processes = 4 #number of processes for multiprocessing
    DOCUMENTS_SIZE = 3 * int(1e6) #size of a group of documents in MB per process
    SINGLE_DOC_SIZE = 1 * int(1e6) #size of a single document per process
    
    corpus = parseString(input_dict['corpus'])
    language = input_dict['lang'], 
    postprocess = input_dict['postprocess'] == "true"
    xml = input_dict['xml'] == "true"

    params = {"language": language, 
            "postprocess": postprocess, 
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

    results, docs, single_docs = [], [], []
    for i, document in enumerate(documents):
        doc_len = len(document.getElementsByTagName('body')[0].getElementsByTagName('p')[0].childNodes[0].nodeValue)
        doc_title = document.getElementsByTagName('title')[0].firstChild.nodeValue
        print doc_title
        if doc_len > SINGLE_DOC_SIZE:
            #split single huge document
            
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
                sub_params["doc_id"] = str(len(results))
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
                    sub_params["doc_id"] = str(len(results))
                    results.append(pool.apply_async(totrtale_request, args=[sub_params]))
                    document_num += 1
                    process_num += 1
                    single_docs.append(2)
            print "document was split",doc_title, len(single_docs)
        else:
            #group multiple smaller documents.
            docs.append(document.toxml())
            document_num+=1
            documents_size += doc_len
            
            if documents_size > DOCUMENTS_SIZE or (document_num) % 10==0 or i == len(documents)-1:
                documents_size = 0
                document_num = 0
                sub_params = copy.deepcopy(params)
                sub_params["text"] = "\n".join(docs)
                sub_params["doc_id"] = str(len(results))
                print "whole document was added", len(docs)
                results.append(pool.apply_async(totrtale_request, args=[sub_params]))
                process_num += 1
                docs = []
                single_docs.append(-1)
    pool.close()

    #we need to join results of totrtale processing back together. Funtion also updates progress bar.
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
                    #results are in xml
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
                    #results are tab separated
                    if single_docs[i] in [0,1]:
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
    
    #return output only if all processes are completed.
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

def get_default_stop_word_list(lang):
    if lang == "en":
        return ["et al", "example", "use", "source", "method", "approach", "table", "figure", "percentage"]
    elif lang == "sl":
        return ["itd", "primer", "uporaba", "vir", "metoda", "pristop", "tabela", "slika", "odstotek"]

def nlp_term_extraction2(input_dict):
    '''
    Term extraction from totrtale annotations.
    '''
    ws_url = webservice_def_ex_url + "/call"
    annotations = input_dict['annotations']
    lang = input_dict['lang']
    stop_list_checkbox = input_dict["stop_list"] == "true"
    user_stop_words = []

    if input_dict['stop_words_file'] != "":
        user_stop_words = safeOpen(input_dict['stop_words_file']).read()
        try:
            user_stop_words.decode("utf-8")
        except Exception:
            raise Exception("Please make sure that your stop words list is encoded in UTF-8.")
        user_stop_words = [word.strip() for word in user_stop_words.split("\n")]

    if '<TEI xmlns="http://www.tei-c.org/ns/1.0">' in annotations:
        annotations = TEItoTab(annotations)
    
    if lang == "sl":
        reference_corpus = input_dict["slovene_reference_corpus"]
    elif lang == "en":
        reference_corpus = input_dict["english_reference_corpus"]
    
    params = {"corpus":annotations,
              "lang": lang,
              "reference_corpus":reference_corpus}
    response = post(ws_url, data=params)
    resp = json.loads(response.content)[u'callResponse'][u'callResult']

    stop_list = []
    if stop_list_checkbox:
        stop_list = get_default_stop_word_list(lang)
    stop_list = set(stop_list + user_stop_words)

    if len(stop_list) > 0:
        resp = resp.split("\n")
        i=0
        while i < len(resp):
            increase = True
            line = resp[i]
            if len(line) > 0:
                term = line.split("\t")[1][1:-1]
                for word in term.split(" "):
                    if word.lower() in stop_list:
                        increase = False
                        resp.pop(i)
                        break
            if increase:
                i+=1
        resp = "\n".join(resp)
    return {'candidates': resp}


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

def nlp_def_extraction_patterns2(input_dict):
    '''
    Definition extraction using pre-defined patterns.
    '''
    annotations = input_dict['annotations']
    lang = input_dict['lang']
    pattern = input_dict['pattern']

    if lang == "sl" and pattern == "begin_allvar":
        raise Exception("Pattern begin_allvar is not supported for slovene language.")


    if '<TEI xmlns="http://www.tei-c.org/ns/1.0">' in annotations:
        annotations = TEItoTab(annotations)

    ws_url = webservice_def_ex_url + "/patDefSent"
    params = {"corpus":annotations,
              "pattern":pattern,
              "lang":lang}
    
    response = post(ws_url, data=params)
    response = json.loads(response.content)[u'patDefSentResponse'][u'patDefSentResult']
    
    return {'sentences': response}

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

def nlp_def_extraction_terms2(input_dict):
    '''
    Definition extraction using terms.
    '''
    annotations = input_dict['annotations']
    term_candidates = input_dict['term_candidates']
    lang = input_dict['lang']
    terms_per_sentence = input_dict['terms_per_sentence']
    nominatives = input_dict['nominatives']
    threshold = input_dict['threshold']
    verb_two_terms = input_dict['verb_two_terms']
    multiword_term = input_dict['multiword_term']
    num_multiterms = input_dict['num_multiterms']
    term_beginning = input_dict['term_beginning']

    if '<TEI xmlns="http://www.tei-c.org/ns/1.0">' in annotations:
        annotations = TEItoTab(annotations)

    ws_url = webservice_def_ex_url + "/termDefSent"
    params = {"corpus":annotations,
              "candidates":term_candidates,
              "lang":lang,
              "nominatives":nominatives,
              "terms_per_sentence":terms_per_sentence,
              "select": threshold,
              "verb_two_terms":verb_two_terms,
              "multiword_term":multiword_term,
              "num_multiterms":num_multiterms,
              "term_beginning":term_beginning}
    response = post(ws_url, data=params)
    response = json.loads(response.content)[u'termDefSentResponse'][u'termDefSentResult']

    return {'sentences': response}


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

def nlp_def_extraction_wnet2(input_dict):
    '''
    Definition extraction using WordNet.
    '''
    annotations = input_dict['annotations']
    lang = input_dict['lang']
    
    if '<TEI xmlns="http://www.tei-c.org/ns/1.0">' in annotations:
        annotations = TEItoTab(annotations)

    ws_url = webservice_def_ex_url + "/wnetDefSent"
    params = {"corpus":annotations,
              "lang":lang}
    response = post(ws_url, data=params)
    response = json.loads(response.content)[u'wnetDefSentResponse'][u'wnetDefSentResult']
    return {'sentences': response}

def TEItoTab(text, doc_id=0):    
    mask1 = ["\tTOK\t", "\t", "\t\n"]
    pattern1 = "<w (type=\"unknown\")| lemma=\"(?P<lemma>.*?)\" ana=\"(?P<ana>.*?)\">(?P<value>.*?)</w>"
    pattern2 = "<title>(.*?)</title>"
    pattern3 = "<pc>(.*?)</pc>"
    
    pattern4 = "(.*?)\t(TOK)\t(.*?)\t(Y)"
    pattern5 = "(.*?)\t(TOK)\t(.*?)\t(Mdo|Mdc)"

    pattern6 = "<w>(.*)</w>"
    newText=[]
    print "TEItoTab started"
    sentence_id = 0
    choice_found=False #if lang in ["gaji", "boho"]
    local_s=""
    for l in text.splitlines():
        print l
        
        if "<choice>" in l:
            choice_found=True
            first = True
            continue
        elif choice_found and "<w" in l:
            local_s = re.findall(pattern6, l)[0]
            choice_found=False
            continue

        if "<w" in l:
            match = [m.group("value", "lemma", "ana") for m in re.finditer(pattern1, l)]
            if len(match) == 0:
                local_s += " " + re.findall(pattern6, l)[0]
            
            elif len(match) == 1:
                match = match[0]
                
            elif len(match) == 2:
                match = match[1]
            l = ''.join(itertools.chain.from_iterable(zip(match, mask1)))
            if len(l) < 100:
                value = re.findall(pattern4, l)
                if len(value) > 0:
                    l = "\t".join(value[0]).replace("TOK", "TOK_ABBR") + "\t\n"

                value = re.findall(pattern5, l)
                if len(value) > 0:
                    l = "\t".join(value[0]).replace("TOK", "TOK_DIG") + "\t\n"
            if len(local_s) > 0:
                l = local_s + "|" + l
                local_s = ""
            newText.append(l)
        elif "<s>" in l:
            newText.append("\t\t<S id=\"" + str(doc_id) + "_" + str(sentence_id) + "\">\t\n")
        elif "</s>" in l:
            newText.append("\t\t</S>\t\n")
            sentence_id+=1
        elif "<pc>" in l:
            value = re.findall(pattern3, l)[0]
            if value == ".":
                newText.append(value+"\t\tPUN_TERM\t\n")
            else:
                value = value.replace("&amp;","&").replace("&lt;","<").replace("&gt;", ">").replace("&quot;","\"")
                newText.append(value+"\t\tPUN\t\n")
        elif "<title>" in l:
            title = re.findall(pattern2, l)[0]
            title = title.replace("&amp;","&").replace("&lt;","<").replace("&gt;", ">").replace("&quot;","\"")
            newText.append("<TEXT title=" + title + ">\t\n")
        elif "</body>" in l:
            newText.append("</TEXT>\t\n")
    return "".join(newText)

def definition_sentences2(input_dict):
    return {}
    
