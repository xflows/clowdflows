from serialization_utils import *

#------------------------------------------------------------------------------
# VISUALISATIONS
#------------------------------------------------------------------------------
def show_adc(input_dict):
    if input_dict["adc"] == None:
        raise Exception("Input ADC is required for displaying Anotated Document Corpus!")
    return {}

def show_clusters(input_dict):
    return {}

def show_classifications(input_dict):
    return {}

def advanced_object_viewer(input_dict):
    return {}

def show_table(input_dict):
    return {}

#------------------------------------------------------------------------------
# SUPPLEMENTARY FUNCTIONS
#------------------------------------------------------------------------------
def split_object(input_dict):
    output_dict = {}
    obj = input_dict['object']
    output_dict['object'] = eval("obj"+input_dict['attribute'])
    return output_dict

def python_snippet(input_dict):
    output_dict = {}
    input = input_dict['in']
    for (i, val) in enumerate(input):
        vars()["in"+str(i+1)] = val
    out1 = None
    exec(input_dict['pycode'])
    output_dict['out'] = out1
    return output_dict

def create_range(input_dict):
    rng = range(ToInt(input_dict['start']), ToInt(input_dict['stop']), ToInt(input_dict['step']))
    return {'range':rng }


