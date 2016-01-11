import time
from subprocess import Popen, PIPE
import os

class Proper(object):
    def __init__(self,input_dict,is_relaggs):
        self.context = input_dict['context'];
        self.result_table = '_%s_%s' % (('relaggs' if is_relaggs else ('quantiles' if 'quantiles_number' in input_dict else 'cardinalize' )), int(round(time.time() * 1000)) )
        self.args_list = self.init_args_list(input_dict,is_relaggs)
    
    def init_args_list(self, input_dict,is_relaggs):
        excluded_fields = self.parse_excluded_fields(input_dict['context'])
        args_list = ['java', '-Xmx512m', '-jar', 'bin/properLauncher.jar']
        if is_relaggs:
            args_list += ['-relaggs']
        else:
            args_list += ['-cardinalizer']
    
    
        args_list += [
                '-use_foreign_keys',
                '-associated_tables', ','.join(set(input_dict['context'].tables).difference(input_dict['context'].target_table)),
                '-user',input_dict['context'].connection.user,
                '-password', input_dict['context'].connection.password, 
                '-result_table', self.result_table, 
                '-driver', input_dict['context'].connection.dal.get_driver_name(),
                '-exclude_fields', excluded_fields,
                '-field',input_dict['context'].target_att,
                '-url', input_dict['context'].connection.dal.get_jdbc_prefix() + input_dict['context'].connection.host + '/',
                '-database', input_dict['context'].connection.database,                    
                '-table', input_dict['context'].target_table]
        
               
        try:
            quantiles_number = input_dict['quantiles_number']
        except KeyError:
            pass
        else:
            try:
                int(quantiles_number)
            except ValueError:
                raise Exception('Number of quantiles should be an integer')
            else:
                args_list += ['-discretize', '1','-discretize-parts', quantiles_number]
    
        return args_list        
        
    def run(self):
        output_dict = {}
        
        p = Popen(self.args_list,cwd=os.path.dirname(os.path.abspath(__file__)), stdout=PIPE)
        stdout_str, stderr_str = p.communicate()
        
        output_dict['context'] = self.context.change_table(self.result_table)        
        return output_dict
    
    def parse_excluded_fields(self, context):   
        excluded_fields = set()
        for table in context.tables:
            excluded_set = set(context.all_cols[table]).difference(set(context.col_vals[table].keys()))
            if excluded_set:
                excluded_fields.add(','.join(map(lambda field: table + '.' + str(field), excluded_set)))
        return ','.join(excluded_fields)
