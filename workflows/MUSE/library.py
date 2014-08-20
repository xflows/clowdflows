import re
import subprocess
import logging
try:
    from ladon.clients.jsonwsp import JSONWSPClient
except ImportError:
    logging.warning('Ladon package not available, most of MUSE stuff will not work.')



def MUSE_annotate(input_dict):
    from NLP_interface import invoke_terence_service
    text = input_dict['raw_text']
    xml = invoke_terence_service(text)
    #xml = xml.replace('\n', ' ')
    #xml = xml.replace('\t', ' ')
    return {'annotations_xml': xml}


def MUSE_view_xml(input_dict):
    return {'xml_data': input_dict.get('xml_data', None)}


def MUSE_mapping_to_KR(input_dict):
    url = input_dict['url']
    data = input_dict['input_data']

    cli = JSONWSPClient(url)
    result = cli.mapping_to_KR(SRL=data)

    return {'mapping': result.response_dict['result']}


def MUSE_mapping_to_KR_latest(input_dict):
    url = input_dict['url']
    data = input_dict['input_data']

    cli = JSONWSPClient(url)
    result, xml = cli.mapping_to_KR_latest(SRL=data)
    mapping, xml = result.response_dict['result']

    return {'mapping': mapping, 'xml': xml}


def MUSE_mapping_to_KR_precomputed(input_dict):
    url = input_dict['url']
    data = input_dict['input_data']

    cli = JSONWSPClient(url)
    result = cli.mapping_to_KR_precomputed(SRL=data)

    return {'mapping': result.response_dict['result']}


def MUSE_mapping_to_KR_precomputed_latest(input_dict):
    url = input_dict['url']
    data = input_dict['input_data']

    cli = JSONWSPClient(url)
    result = cli.mapping_to_KR_precomputed_latest(SRL=data)
    mapping, xml = result.response_dict['result']

    return {'mapping': mapping, 'xml': xml}


def MUSE_semantic_role_labeling(input_dict):
    url = input_dict['url']
    data = input_dict['xml']

    cli = JSONWSPClient(url)
    result = cli.semantic_role_labeling(xml=data)
    srl, txt = result.response_dict['result']

    return {'xml': srl, 'txt': txt}


def MUSE_semantic_role_labeling_tuk(input_dict):
    url = input_dict['url']
    data = input_dict['xml']

    cli = JSONWSPClient(url)
    result = cli.semantic_role_labeling_tuk(xml=data)
    srl, txt = result.response_dict['result']

    return {'xml': srl, 'txt': txt}


def MUSE_virtual_environment_visualization(input_dict):
    NLP_data = input_dict.get('NLP_data', None)
    unitylink = input_dict.get('unitylink', 'https://ive.scm.tees.ac.uk/muse/tukdemo/')
    return {'NLP_data' : NLP_data,
            'unitylink': unitylink}