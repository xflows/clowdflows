from subprocess import Popen, PIPE
import os
import re
import helper

class Tertius(object):
    def __init__(self,input_dict):
        self.prd_file_path = input_dict['prd_file']
        self.url = os.path.normpath(re.sub('\..*$', '', self.prd_file_path))
        self.test_file_path = input_dict['test_file']
        self.fct_file_path = input_dict['fct_file']
        self.args_list = self.init_args_list(input_dict)
        
    def init_args_list(self, input_dict):           
        args_list = [os.path.join(os.path.dirname(os.path.abspath(__file__)),os.path.join('bin','tertius.exe' if os.name == 'nt' else 'tertius'))]
        args_list += ['-m', '512']
        
        try:
            max_literal = int(input_dict['max_literal'])
        except ValueError:
            raise Exception('Max Literal should be an integer')
        
        try:
            max_variable = int(input_dict['max_variable'])
        except ValueError:
            raise Exception('Max Variable should be an integer')
    
        try:
            struct_nb_properties = int(input_dict['struct_nb_properties'])
        except ValueError:
            pass
        else:
            if struct_nb_properties >= 0:
                args_list += ['-struct', str(struct_nb_properties)]   
    
        try:
            nb_results = int(input_dict['nb_results'])
        except ValueError:
            nb_results = -1
        else:
            if nb_results > 0:
                args_list += ['-k', str(nb_results)]   
             
        if nb_results < 0:
            try:
                conf_thres = int(input_dict['conf_thres'])
            except ValueError:
                conf_thres = -1
            else:
                if conf_thres > 0:
                    args_list += ['-c', str(conf_thres)]   
                    
        if input_dict['count_bottom_up']:
            args_list += ['-cbu']                       
    
        if input_dict['sat_clauses']:
            args_list += ['-sat'] 
            
        try:
            noise_percent_thres = float(input_dict['noise_percent_thres'])
        except ValueError:
            pass
        else:
            if noise_percent_thres >= 0:
                args_list += ['-n', str(noise_percent_thres)]   
                
        if str(input_dict['lang_bias']) == 'horn':
            args_list += ['-b', 'horn']   
        elif str(input_dict['lang_bias']) == 'horn_pos_class':
            args_list += ['-b', 'horn_pos_class']   
        elif str(input_dict['lang_bias']) == 'pos_class':
            args_list += ['-b', 'pos_class']   
        elif str(input_dict['lang_bias']) == 'class':
            args_list += ['-b', 'class']   
                               
        att_list = helper.create_attribute_list(input_dict['attribute_list'])
        if att_list:
            args_list += att_list  
                                
        args_list += [str(max_literal) + '/' + str(max_variable),os.path.relpath(self.url, os.path.dirname(os.path.abspath(__file__)))]
        
        return args_list
        
    def run(self):
        output_dict = {}
        
        helper.handle_files(self.fct_file_path, self.test_file_path, self.url)
        
        p = Popen(self.args_list,cwd=os.path.dirname(os.path.abspath(__file__)), stdout=PIPE)   
        stdout_str, stderr_str = p.communicate()       
    
        with open(self.url+'.res') as f:
            res_file = f.read()
    
        output_dict['results'] = res_file
                       
        return output_dict
