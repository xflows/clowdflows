from workflows.security import safeOpen
import json
import sys

from workflows import module_importer
def setattr_local(name, value, package):
    setattr(sys.modules[__name__], name, value)
module_importer.import_all_packages_libs("library",setattr_local)

def test_interaction(input_dict):
    return input_dict

def add_multiple(input_dict):
    output_dict = {}
    output_dict['sum'] = 0
    for i in input_dict['integer']:
        output_dict['sum'] = float(i)+output_dict['sum']
    return output_dict

def load_file(input_dict):
    return input_dict
    
def file_to_string(input_dict):
    f = safeOpen(input_dict['file'])
    output_dict = {}
    output_dict['string']=f.read()
    return output_dict

def load_to_string(input_dict):
    '''
    Opens the file and reads its contents into a string.
    '''
    f = safeOpen(input_dict['file'])
    output_dict = {}
    output_dict['string']=f.read()
    return output_dict

def call_webservice(input_dict):
    from services.webservice import WebService
    ws = WebService(input_dict['wsdl'],float(input_dict['timeout']))
    selected_method = {}
    for method in ws.methods:
        if method['name']==input_dict['wsdl_method']:
            selected_method = method
    function_to_call = getattr(ws.client,selected_method['name'])
    ws_dict = {}
    for i in selected_method['inputs']:
        try:
            ws_dict[i['name']]=input_dict[i['name']]
            if ws_dict[i['name']] is None:
                pass
            if i['type'] == bool:
                if input_dict[i['name']]=="true":
                    ws_dict[i['name']]=1
                else:
                    ws_dict[i['name']]=0
            if ws_dict[i['name']] == '':
                if input_dict['sendemptystrings']=="true":
                    ws_dict[i['name']] = ''
                else:
                    ws_dict.pop(i['name'])
        except Exception as e: 
            print e
            ws_dict[i['name']]=''
    results = function_to_call(**ws_dict)
    output_dict=results
    if type(results)==dict:
        return output_dict
    elif type(results)==list:
        output_dict = {}
        for l in results:
            if type(l)==dict:
                for k in l.keys():
                    a = output_dict.get(k,[])
                    a.append(l[k])
                    output_dict[k]=a
        return output_dict
    return results

def multiply_integers(input_dict):
    product = 1
    for i in input_dict['integers']:
        product = product*int(i)
    output_dict={'integer':product}
    return output_dict

def filter_integers(input_dict):
    return input_dict
    
def filter_integers_post(postdata,input_dict,output_dict):
    try:
        output_dict['integers'] = postdata['integer']
    except:
        pass
    return output_dict

def create_integer(input_dict):
    output_dict = {}
    output_dict['integer'] = input_dict['integer']
    return output_dict
    
def create_string(input_dict):
    return input_dict  
    
def concatenate_strings(input_dict):
    output_dict = {}
    j = len(input_dict['strings'])
    for i in range(j):
        input_dict['strings'][i]=str(input_dict['strings'][i])
    output_dict['string'] = input_dict['delimiter'].join(input_dict['strings'])
    return output_dict
    
def display_string(input_dict):
    return {}

def add_integers(input_dict):
    output_dict = {}
    output_dict['integer'] = int(input_dict['integer1'])+int(input_dict['integer2'])
    return output_dict

def table_viewer(input_dict):
    return {}

def subtract_integers(input_dict):
    output_dict = {}
    output_dict['integer'] = int(input_dict['integer1'])-int(input_dict['integer2'])
    return output_dict
    
def select_attrs(input_dict):
    return input_dict

def select_attrs_post(postdata, input_dict, output_dict):
    import Orange
    
    data = Orange.data.Table(input_dict['data'])
    
    new_attrs = []
    for name in postdata['attrs']:
        new_attrs.append(str(name))
    
    try:
        new_attrs.append(str(postdata['ca'][0]))
        class_attr = True
    except:
        class_attr = False

    new_domain = Orange.data.Domain(new_attrs, class_attr, data.domain)

    try:
        for meta in postdata['ma']:
            if data.domain.has_meta(str(meta)):
                new_domain.addmeta(Orange.feature.Descriptor.new_meta_id(), data.domain.getmeta(str(meta)))
            else:
                new_domain.add_meta(Orange.feature.Descriptor.new_meta_id(), data.domain[str(meta)])
    except:
        pass    

    new_data = Orange.data.Table(new_domain, data)

    output_dict = {'data':new_data}
    return output_dict

def select_data(input_dict):
    return input_dict

def build_filter(val, attr, data):
    import Orange

    pos = 0
    try:
        pos = data.domain.meta_id(attr)
    except Exception, e:
        pos = data.domain.variables.index(attr)

    if val['operator'] == ">":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                ref = float(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Greater
            )
        )
    elif val['operator'] == "<":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                ref = float(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Less
            )
        )
    elif val['operator'] == "=":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                ref = float(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Equal
            )
        )
    elif val['operator'] == "<=":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                ref = float(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.LessEqual
            )
        )
    elif val['operator'] == ">=":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                ref = float(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.GreaterEqual
            )
        )
    elif val['operator'] == "between":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                min = float(val['values'][0]),
                max = float(val['values'][1]),
                oper = Orange.data.filter.ValueFilter.Between
            )
        )
    elif val['operator'] == "outside":
        return(
            Orange.data.filter.ValueFilterContinuous(
                position = pos,
                min = float(val['values'][0]),
                max = float(val['values'][1]),
                oper = Orange.data.filter.ValueFilter.Outside
            )
        )
    elif val['operator'] in ["equals", "in"]:
        vals=[]
        for v in val['values']:
            vals.append(Orange.data.Value(attr, str(v)))
        return(
            Orange.data.filter.ValueFilterDiscrete(
                position = pos,
                values=vals
            )
        )
    elif val['operator'] == "s<":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Less,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "s>":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Greater,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "s=":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Equal,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "s<=":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.LessEqual,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "s>=":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.GreaterEqual,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "sbetween":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                min = str(val['values'][0]),
                max = str(val['values'][1]),
                oper = Orange.data.filter.ValueFilter.Between,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "soutside":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                min = str(val['values'][0]),
                max = str(val['values'][1]),
                oper = Orange.data.filter.ValueFilter.Outside,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "scontains":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.Contains,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "snot contains":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.NotContains,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "sbegins with":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.BeginsWith,
                case_sensitive = bool(val['case'])
            )
        )
    elif val['operator'] == "sends with":
        return(
            Orange.data.filter.ValueFilterString(
                position = pos,
                ref = str(val['values'][0]),
                oper = Orange.data.filter.ValueFilter.EndsWith,
                case_sensitive = bool(val['case'])
            )
        )



def send_filename(input_dict):
    output_dict = {}
    output_dict['filename']=input_dict['fileloc'].strip('\"').replace('\\', '\\\\')
    return output_dict
    



def alter_table(input_dict):
    return {'altered_data' : None}

def alter_table_finished(postdata, input_dict, output_dict):
    import Orange
    from Orange.feature import Type
    from visualization_views import orng_table_to_dict
    widget_id = postdata['widget_id'][0]
    # Parse the changes
    altered_cells = json.loads(postdata['alteredCells'+widget_id][0])
    new_table = Orange.data.Table(input_dict['data'])
    for cell, new_value in altered_cells.items():
        tokens = cell.split('_')
        inst_idx, att = int(tokens[1]), str(tokens[2])
        if new_table[inst_idx][att].var_type == Type.Continuous:
            new_table[inst_idx][att] = float(new_value)
        else: # Discrete or string 
            # TODO: 
            # This raises an exception if new_value is not among the legal values for the discrete attribute
            # - add a dropdown list of legal values when editing the table!
            try:
                new_table[inst_idx][att] = str(new_value)
            except: # Catch orange exception and give a proper error message.
                raise Exception("Illegal value '%s' for discrete attribute '%s', legal values are: %s." % (new_value, att, new_table.domain[att].values))
    return {'altered_data' : new_table}

def tree_visualization(input_dict):
    return{}

def example_distance(input_dict):
    return input_dict


# import hashlib
# def hash_it(input_dict):
#    output_dict = {}
#    output_dict["output1"] = hashlib.sha256(input_dict["input1"]).hexdigest()
#    output_dict["numLoop"] = input_dict["numLoop"]
#    for i in range(1,input_dict["numLoop"]):
#        output_dict["output1"] = hashlib.sha256(output_dict["output1"]).hexdigest();
#    return output_dict
   