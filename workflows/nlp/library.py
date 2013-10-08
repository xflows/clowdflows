import nlp
import os.path
import base64
from services.webservice import WebService
from workflows.security import safeOpen

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
    f = safeOpen(input_dict['file'])
    fname = os.path.basename(input_dict['file'])
    data = base64.b64encode(f.read())
    ws = WebService('http://vihar.ijs.si:8095/totale?wsdl', 60000)
    response = ws.client.parseFile(fileName=fname, inFile=data)
    return {'corpus' : response['parsedFile']}
