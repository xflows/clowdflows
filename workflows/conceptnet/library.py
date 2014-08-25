import re
import json
import sys

def conceptnet_create_integers(input_dict):
    intStr = input_dict['intStr']
    intList = []
    for i in re.findall(r'\w+', intStr):
        try:
            intList.append(int(i))
        except:
            pass
    if input_dict['sort'].lower() == "true":
        intList.sort()
    return {'intList':intList}

def conceptnet_sum_integers(input_dict):
    intList = input_dict['intList']
    return {'sum':sum(intList)}

def conceptnet_pre_filter_integers(input_dict):
    return input_dict

def conceptnet_post_filter_integers(postdata,input_dict,output_dict):
    intListOut = postdata['intListOut']
    intList = []
    for i in intListOut:
        try:
            intList.append(int(i))
        except:
            pass
    return {'intList': intList}

def conceptnet_pre_display_summation(input_dict):
    return {}

def createBlaString():
    return "bla";

import hashlib

def hash_it(input_dict):
    output_dict = {}
    output_dict["output1"] = hashlib.sha256(input_dict["input1"]).hexdigest()
    output_dict["numLoop"] = input_dict["numLoop"]
    for i in range(1,input_dict["numLoop"]):
        output_dict["output1"] = hashlib.sha256(output_dict["output1"]).hexdigest();
    return output_dict

def query_to_json(input_dict):
    import urllib2, json

    site = "http://conceptnet5.media.mit.edu/data/5.2/"+input_dict["query"];
    output_dict = {}

    response = urllib2.urlopen(site)
    json_file = response.read();
    js = json.loads(json_file);
    output_dict["json"] = js;

    return output_dict;


def create_conceptnet_query(input_dict):
    print "create_conceptnet_query"
    input_command = input_dict['command']
    input_limit = input_dict['limit']
    input_offset = input_dict['offset']

    query = "c/en/"+ input_command +"?offset="+str(input_limit)+"&limit="+str(input_offset)
    output_dict["query"] = query;


    return output_dict;

def create_lookup_query(input_dict):
    print "create_lookup_query"

    input_limit = input_dict['limit']
    input_offset = input_dict['offset']

    query = "c/en/toast?"

    if input_limit != None and input_limit != '':
        query = query + 'limit=' + str(input_limit) + '&'
    if input_offset != None and input_offset != '':
        query = query + 'offset=' + str(input_offset) + '&'

    output_dict = {}
    output_dict["query"] = query[0:len(query)-1]

    return output_dict;

def create_search_query(input_dict):
    print "create_conceptnet_query"

    input_limit = input_dict['limit']
    input_offset = input_dict['offset']
    input_text= input_dict['text']
    input_minweight = input_dict['minweight']

    query = "c/en/search?"

    if input_limit != None and input_limit != '':
        query = query + 'limit=' + str(input_limit) + '&'
    if input_offset != None and input_offset != '':
        query = query + 'offset=' + str(input_offset) + '&'
    if input_text != None and input_text != '':
        query = query + 'text=' + str(input_text) + '&'
    if input_minweight != None and input_minweight != '':
        query = query + 'minWeight=' + str(input_minweight) + '&'

    
    output_dict = {}
    #query = "5.2/c/en/search?offset="+str(input_limit)+"&limit="+str(input_offset)
    output_dict["query"] = query[0:len(query)-1];

    return output_dict;

def create_association_query(input_dict):
    print "create_conceptnet_query"
    input_command = input_dict['command']
    input_limit = input_dict['limit']
    input_offset = input_dict['offset']

    query = "c/en/"+ input_command +"?offset="+str(input_limit)+"&limit="+str(input_offset)
    output_dict["query"] = query;

    return output_dict;


def create_integer_list(input_dict):
    import re
    intStr = input_dict['intStr']
    intList = []
    for i in re.findall(r'\w+', intStr):
        try:
            intList.append(int(i))
        except:
            pass
    if input_dict['sort'].lower() == "true":
        intList.sort()
    return {'intList':intList}