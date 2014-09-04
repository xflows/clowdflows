import re
import json
import sys


def conceptnet_execute_query(input_dict):
    """
    Executes the input Conceptnet query.
    """
    import urllib2
    site = "http://conceptnet5.media.mit.edu/data/5.2/" + input_dict["query"]
    response = urllib2.urlopen(site)
  
    output_dict = {}
    output_dict["json"] = response.read()
    return output_dict


def conceptnet_create_search_query(input_dict):
    """
    Constructs a Conceptnet search query.
    """
    input_limit = input_dict['limit']
    input_offset = input_dict['offset']
    input_text = input_dict['text']
    input_minweight = input_dict['minweight']
    
    query = "search?"

    if input_limit != None and input_limit != '':
        query = query + 'limit=' + str(input_limit) + '&'
    if input_offset != None and input_offset != '':
        query = query + 'offset=' + str(input_offset) + '&'
    if input_text != None and input_text != '':
        query = query + 'text=' + str(input_text) + '&'
    if input_minweight != None and input_minweight != '':
        query = query + 'minWeight=' + str(input_minweight) + '&'

    output_dict = {}
    output_dict["query"] = query[0:len(query)-1]
    return output_dict


def concept_net_triplets(input_dict):
    """
    Extracts Subject, Predicate, Object triplets from the Conceptnet response JSON.
    """
    json_response = input_dict['concept_net_json_response']
    try:
        data = json.loads(json_response)
        triplets = [[e['start'], e['rel'], e['end']] for e in data['edges']]
    except:
        raise Exception("Cannot parse this JSON string. Is it a proper Conceptnet API response JSON?")
    
    # Make a separate triplet list without conceptnet prefixes
    basename_triplets = []
    for start, rel, end in triplets:
        start = start.split('/')[-1]
        rel = rel.split('/')[-1]
        end = end.split('/')[-1]
        basename_triplets.append([start, rel, end])

    output_dict = {}
    output_dict['triplets'] = json.dumps(triplets, indent=2)
    output_dict['basename_triplets'] = json.dumps(basename_triplets, indent=2)
    return output_dict
