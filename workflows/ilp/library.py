import re
from string import ascii_uppercase as chars, digits
from random import choice

from aleph import aleph

def ilp_pre_aleph(input_dict):
    return input_dict

def ilp_post_aleph(postdata, input_dict, output_dict):
    settings = input_dict['settings']
    mode = input_dict['mode']
    pos = input_dict['pos']
    neg = input_dict['neg']
    b = input_dict['b']
    # Random 10 character experiment id.
    eid = ''.join(choice(chars + digits) for i in range(10))
    result = aleph.induce(mode, eid, pos, neg, b)
    return {'result' : result}