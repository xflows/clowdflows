from subprocess import Popen, PIPE
import os
import re
import helper

class OneBC(object):
    def __init__(self,input_dict,is1BC2):
        self.prd_file_path = input_dict['prd_file']
        self.url = os.path.normpath(re.sub('\..*$', '', self.prd_file_path))
        self.test_file_path = input_dict['test_file']
        self.fct_file_path = input_dict['fct_file']
        self.is1BC2 = is1BC2;
        self.args_list = self.init_args_list(input_dict)
    
    def init_args_list(self, input_dict):                 
        args_list = [os.path.join(os.path.dirname(os.path.abspath(__file__)),os.path.join('bin','1BC.exe' if os.name == 'nt' else '1BC')), '-r', 'INDIVIDUAL' if self.is1BC2 else 'LANGUAGE']
        args_list += ['-roc']
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
            cross_number = int(input_dict['cross_number'])
        except ValueError:
            pass
        else:
            args_list += ['-cross', str(cross_number)]
            
        try:
            srand = int(input_dict['srand'])
        except ValueError:
            pass
        else:
            args_list += ['-srand', str(srand)]
            
        try:
            roc_nb_folds = int(input_dict['roc_nb_folds'])
        except ValueError:
            pass
        else:
            if roc_nb_folds >= 0:
                args_list += ['-o', str(roc_nb_folds)]   
                
        if input_dict['load_part_inc']:
            args_list += ['-i']                 
    
        att_list = helper.create_attribute_list(input_dict['attribute_list'])
        if att_list:
            args_list += att_list   
      
        args_list += ['class']
        
        args_list += [str(max_literal) + '/' + str(max_variable), os.path.relpath(self.url, os.path.dirname(os.path.abspath(__file__)))]    
        
        return args_list
           
    def run(self):
        output_dict = {}
        
        helper.handle_files(self.fct_file_path, self.test_file_path, self.url)

        p = Popen(self.args_list,cwd=os.path.dirname(os.path.abspath(__file__)), stderr=PIPE, stdout=PIPE)   
        stdout_str, stderr_str = p.communicate()      
    
        with open(self.url + '.res') as f:
            res_file = f.read()
    
        output_dict['results'] = res_file
        print stdout_str
        print ' '.join(self.args_list)
        print stderr_str
        output_dict['score'] = stderr_str
            
        return output_dict