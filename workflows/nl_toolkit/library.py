import re

def nl_toolkit_get_all_synsets(input_dict):
    if input_dict['corpus'].lower()=='wordnet':
        from nltk.corpus import wordnet as wn
        synsets = {}
        for word in input_dict['words']:
            synsets[word] = wn.synsets(word)
        return {'synsets' : synsets}

def nl_toolkit_get_word_synsets(input_dict):
    if input_dict['corpus'].lower()=='wordnet':
        from nltk.corpus import wordnet as wn
        return {'synsets':wn.synsets(input_dict['word'])}

def nl_toolkit_create_integers(input_dict):
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

def nl_toolkit_sum_integers(input_dict):
    intList = input_dict['intList']
    return {'sum':sum(intList)}

def nl_toolkit_pre_filter_integers(input_dict):
    return input_dict

def nl_toolkit_post_filter_integers(postdata,input_dict,output_dict):
    intListOut = postdata['intListOut']
    intList = []
    for i in intListOut:
        try:
            intList.append(int(i))
        except:
            pass
    return {'intList': intList}

def nl_toolkit_pre_display_summation(input_dict):
    return {}