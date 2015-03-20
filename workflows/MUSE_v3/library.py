import re
import subprocess
import logging
try:
    from ladon.clients.jsonwsp import JSONWSPClient
except ImportError:
    logging.warning('Ladon package not available, most of MUSE stuff will not work.')



def MUSE_preprocessing(input_dict):
    url = input_dict['url']
    text = input_dict['text']

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
    entities = input_dict['entities']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.direct_speech(srl=srl, coref=coref, entities=entities)
    dspeech = result.response_dict['result']
    return {'directspeech': dspeech}
#end
















