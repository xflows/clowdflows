import re


def MUSE_annotate(input_dict):
    from NLP_interface import invoke_terence_service
    text = input_dict['raw_text']
    xml = invoke_terence_service(text)
    #xml = xml.replace('\n', ' ')
    #xml = xml.replace('\t', ' ')
    return {'annotations_xml': xml}


def MUSE_view_xml(input_dict):
    return {'xml_data': input_dict.get('xml_data', None)}
