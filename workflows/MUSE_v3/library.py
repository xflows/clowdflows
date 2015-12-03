import re
import subprocess
import logging
try:
    from ladon.clients.jsonwsp import JSONWSPClient
except ImportError:
    logging.warning('Ladon package not available, most of MUSE stuff will not work.')



def MUSE_preprocessing(input_dict):
    url = input_dict['url']
    text = input_dict['text'].strip()

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.preprocessing(text=text)
    tokens = result.response_dict['result']
    return {'tokens': tokens}
#end


def MUSE_coreference(input_dict):
    url = input_dict['url']
    tokens = input_dict['tokens']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.coreference(tokens=tokens)
    corefs = result.response_dict['result']
    return {'coreferences': corefs}
#end


def MUSE_SRL(input_dict):
    url = input_dict['url']
    tokens = input_dict['tokens']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.SRL(tokens=tokens)
    srl = result.response_dict['result']
    return {'srl': srl}
#end


def MUSE_directspeech(input_dict):
    url = input_dict['url']
    srl = input_dict['srl']
    coref = input_dict['coref']
    entities = input_dict['entities'].replace(' ', '')

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.direct_speech(srl=srl, coref=coref, entities=entities)
    dspeech = result.response_dict['result']
    return {'directspeech': dspeech}
#end


def MUSE_pronoun_resolution(input_dict):
    url = input_dict['url']
    srl = input_dict['srl']
    coref = input_dict['coref']
    entities = input_dict['entities'].replace(' ', '')

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.pronoun_resolution(srl=srl, coref=coref, entities=entities)
    srlp = result.response_dict['result']
    return {'srlpronouns': srlp}
#end


def MUSE_prepare_mapping(input_dict):
    url = input_dict['url']
    srlp = input_dict['srlp']
    quots = input_dict['quots']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.prepare_for_mapping(srlpronouns=srlp, quotations=quots)
    imapping = result.response_dict['result']
    return {'imapping': imapping}
#end


def MUSE_event_temprel_detection(input_dict):
    text = input_dict['text'].strip()
    url = input_dict['url']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.event_temprel_detection(text=text)
    result = result.response_dict['result']
    return {'xml': result}
#end


def MUSE_PG_preprocessing(input_dict):
    text = input_dict['text'].strip()
    url = input_dict['url']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.PG_preprocessing(text=text)
    result = result.response_dict['result']
    return {'structure': result[0], 'tokens': result[1]}
#end


def MUSE_PG_SRL(input_dict):
    url = input_dict['url']
    tokens = input_dict['tokens']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.PG_SRL(tokens=tokens)
    srl = result.response_dict['result']
    return {'srl': srl}
#end


def MUSE_PG_coreference(input_dict):
    url = input_dict['url']
    tokens = input_dict['tokens']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.PG_coreference(tokens=tokens)
    coref, preproc = result.response_dict['result']
    return {'coref': coref, 'preproc': preproc}
#end


def MUSE_PG_mappingVWR(input_dict):
    url = input_dict['url']
    srl = input_dict['srl']
    coref = input_dict['coref']
    info = input_dict['info']
    events = input_dict['events']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.PG_mappingVWR(srl=srl, coref=coref, info=info, events=events)
    mapping = result.response_dict['result']

    return {'mapping': mapping}
#end


def MUSE_PG_event_temprel_detection(input_dict):
    url = input_dict['url']
    preproc = input_dict['preproc'].strip()

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.PG_event_temprel_detection(preprocessed=preproc)
    result = result.response_dict['result']
    return {'xml': result}
#end


def MUSE_PG_GDEE_text(input_dict):
    url = input_dict['url']
    text = input_dict['text']
    lang = input_dict['lang']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.PG_GDEE_text(text=text, lang=lang)
    result = result.response_dict['result']
    return {'xml': result}
#end


def MUSE_PG_GDEE_path(input_dict):
    url = input_dict['url']
    path = input_dict['path']
    lang = input_dict['lang']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.PG_GDEE_path(path=path, lang=lang)
    result = result.response_dict['result']
    return {'xml': result}
#end



#### COPIED FROM OLD MUSE PACKAGE

def MUSE_mapping_to_KR_precomputed_V3(input_dict):
    url = input_dict['url']
    data = input_dict['input_data']

    import socket
    cli = JSONWSPClient(url)
    result = cli.mapping_to_KR_precomputed(SRL=data)
    mapping, xml = result.response_dict['result']

    return {'mapping': mapping, 'xml': xml}
#end



def MUSE_mapping_to_KR_V3(input_dict):
    url = input_dict['url']
    data = input_dict['input_data']

    import socket
    cli = JSONWSPClient(url)
    socket.setdefaulttimeout(None)

    result = cli.mapping_to_KR(SRL=data)
    mapping, xml = result.response_dict['result']
    return {'mapping': mapping, 'xml': xml}
#end



def MUSE_string_to_file_V3(input_dict):
    return {}


def MUSE_string_to_file_finished_V3(postdata, input_dict, output_dict):
    from socket import getfqdn
    import sys

    if len(sys.argv) > 1:
        if len(sys.argv) > 2 and '.' in sys.argv[2] and ':' in sys.argv[2]:
            port = sys.argv[2].split(':')[1]
        else:
            port = 8000
        DEVSERVER = sys.argv[1].startswith('runserver')
    else:
        DEVSERVER = False
    fqdn = '127.0.0.1:' + str(port) if DEVSERVER else getfqdn()
    # dirty,dirty
    if fqdn == 'workflow':
        fqdn = 'workflow.ijs.si'

    fqdn = 'http://' + fqdn

    fileURL = fqdn + postdata.get('fileURL')[0]
    return {'fileURL': fileURL}
#end


def MUSE_view_xml_V3(input_dict):
    return {'xml_data': input_dict.get('xml_data', None)}


def MUSE_virtual_environment_demonstrator_tuk_V3(input_dict):
    return {'mappingLink': str(input_dict['mappingLink']), 'unityLink': input_dict['unityLink']}


def MUSE_virtual_environment_demonstrator_tuk_local_V3(input_dict):
    return {'mappingLink': str(input_dict['mappingLink']), 'unityLink': input_dict['unityLink']}




