import re
from string import ascii_lowercase as chars
from random import choice

from aleph import Aleph
from rsd import RSD

def ilp_aleph(input_dict):
    aleph = Aleph()
    settings = input_dict['settings']
    mode = input_dict['mode']
    pos = input_dict['pos']
    neg = input_dict['neg']
    b = input_dict['b']
    # Parse settings
    if settings:
        aleph.settingsAsFacts(settings)
    # Run aleph
    theory = aleph.induce(mode, pos, neg, b)
    return {'theory' : theory}

def ilp_rsd(input_dict):
    rsd = RSD()
    settings = input_dict.get('settings',None)
    pos = input_dict.get('pos', None)
    neg = input_dict.get('neg', None)
    examples = input_dict.get('examples', None)
    b = input_dict['b']
    subgroups = True if input_dict['subgroups'] == 'true' else False
    # Parse settings
    if settings:
        rsd.settingsAsFacts(settings)
    # Run rsd
    features, arff, rules = rsd.induce(b, examples=examples, pos=pos, neg=neg, cn2sd=subgroups)
    return {'features' : features, 'arff' : arff, 'rules' : rules}
