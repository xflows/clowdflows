import time
import re
import os
import sys
from subprocess import Popen, PIPE
import threading

illegal_predicates = [
    ('library(?!\(myddas\))', 'library'), ('use_module(?!\(library\(myddas\)\))', 'use_module'), ('\[.+\]', '[]'), \
    'abort', 'absolute_file_name', 'access', 'access_file', 'add_to_path', 'alarm', 'alias', 'always_prompt_user', 'at_end_of_stream', \
    'autoload', 'b_getval', 'b_setval', 'bb_delete', 'bb_get', 'bb_put', 'bb_update', 'call', 'catch', 'cd', 'clause', 'close', 'codes', \
    'compilation_mode', 'compile', 'consult', 'create_prolog_flag', 'current_atom', 'current_char_conversion', 'current_host', 'current_input', \
    'current_key', 'current_line_number', 'current_op', 'current_output', 'current_predicate', 'current_stream', 'cycles', 'db_assert', \
    'db_close', 'db_datalog_describe', 'db_datalog_show_tables', 'db_describe', 'db_get_attributes_types', 'db_insert', 'db_module', \
    'db_my_result_set', 'db_my_sql_mode', 'db_number_of_fields', 'db_show_tables', 'db_sql', 'db_top_level', 'db_verbose', 'db_view', \
    'derived_from', 'direct_cycle', 'discontiguous', 'display', 'dynamic', 'end_of_stream', 'ensure_loaded', 'environ', 'eof_action', \
    'erase', 'eraseall', 'erased', 'exception', 'exists', 'exited', 'expand', 'expand_exprs', 'expand_filename', 'extensions', 'file_base_name', \
    'file_errors', 'file_name', 'file_name_extension', 'file_search_path', 'file_type', 'flush_output', 'force', 'format', 'garbage_collect', \
    'garbage_collect_atoms', 'gc', 'get_code', 'get_value', 'getcwd', 'grow_heap', 'grow_stack', 'halt', 'hide', 'hide_predicate', \
    'hostname_address', 'idb', 'ignore_ops', 'imports', 'include', 'initialization', 'insert', 'instance', 'is_stream', 'key_statistics', \
    'library_directory', 'line_count', 'line_position', 'listing', 'load_files', 'max_depth', 'message_hook', 'message_to_string', \
    'must_be_module', 'nb_current', 'nb_delete', 'nb_getval', 'nb_linkarg', 'nb_linkval', 'nb_set_shared_arg', 'nb_set_shared_val', \
    'nb_setarg', 'nb_setval', 'no_style_check', 'nogc', 'nth_instance', 'null', 'numbervars', 'on_signal', 'open', 'path', 'peek_byte', \
    'peek_char', 'peek_code', 'phrase', 'portray', 'portray_clause', 'portrayed', 'predicate_property', 'print', 'print_message', \
    'print_message_lines', 'printf', 'priority', 'prolog_file_name', 'prolog_flag', 'prolog_initialization', 'prolog_load_context', 'prompt', \
    'pthread_setconcurrency', 'put', 'put_byte', 'put_char', 'put_code', 'putenv', 'quoted', 'read', 'read_and_increment_counter', 'read_term', \
    'reconsult', 'recorda', 'recorda_at', 'recordaifnot', 'recorded', 'recordz', 'recordz_at', 'recordzifnot', 'reexport', 'relative_to', \
    'remove_from_path', 'rename', 'reposition', 'representation_errors', 'restore', 'retract', 'save', 'save_program', 'see', 'seeing', \
    'set_input', 'set_output', 'set_prolog_flag', 'set_stream_position', 'set_value', 'sh', 'silent', 'singletons', 'skip', 'socket', \
    'socket_accept', 'socket_bind', 'socket_buffering', 'socket_close', 'socket_connect', 'socket_listen', 'socket_select', 'solutions', \
    'source_file', 'source_location', 'source_mode', 'stream', 'stream_position', 'stream_position_data', 'stream_property', 'stream_select', \
    'style_check', 'syntax_errors', 'system', 'system_predicate', 'tableInfo', 'tell', 'telling', 'term_position', 'thread_at_exit', \
    'thread_create', 'thread_detach', 'thread_exit', 'thread_join', 'thread_self', 'thread_setconcurrency', 'thread_sleep', 'throw', 'time_file', \
    'ttyget', 'ttyget0', 'ttyput', 'ttyskip', 'ttytab', 'unhide', 'unix', 'var', 'variable_names', 'variables', 'view', 'with_output_to', \
    'write', 'write_canonical', 'write_depth', 'write_many_as', 'write_term', 'writeln', 'writeq', 'yap_flag'
]

allowed_mysql_predicates = [
    'use_module(library(myddas))', 
    'db_import', 
    'db_open',
]
default_timeout = 120000 * 60

class SafePopen(threading.Thread):
    '''
    Executes a given command and kills it if it is not done in 'timeout' seconds.

    TODO: run child in some sort of jail.
    '''
    def __init__(self, args, **kwargs):
        threading.Thread.__init__(self)
        self.timeout = kwargs.pop('timeout') if kwargs.has_key('timeout') else default_timeout
        self.args = args
        self.kwargs = kwargs
    
    def run(self):
        self.p = Popen(self.args, **self.kwargs)
        self.p.wait()

    def safe_run(self):
        self.start()
        self.join(self.timeout)
        if self.is_alive():
            self.p.terminate()
            self.join()
            raise Exception('Your computation has exceeded the maximum available time of %.2f minutes.' % (self.timeout / 60.))
        return self.p

def check_input(prolog_file):
    '''
    Check for illegal predicates (like reading/writing, opening sockets, etc).
    '''
    if prolog_file == None:
        return
    for pred in illegal_predicates:
        if type(pred) == tuple:
            print_name = pred[1]
            pred = pred[0]
        else:
            print_name = pred
        if re.search(r'[^\w]' + pred + r'\s*[\(\)\:\.\,\;]+', prolog_file):
            raise Exception('Illegal predicate "%s" used in your input, aborting. If your own predicate clashes with a predefined YAP predicate, you must rename it.' % print_name)

if __name__ == '__main__':
    nasty_scripts = [
        ":- foo,socket_connect('sock',83,stream),foo.", \
        ":- foo,    sh,    foo.", \
        "do_nasty(_) :- open('fn').",\
        ":- true;system('ls -l').", \
        "foo:-p,['another_nasty_script'],q.",
        ":-use_module(library(assoc)).",
    ]
    errors = 0
    for script in nasty_scripts:
        try:
            check_input(script)
        except:
            errors += 1
    assert errors == len(nasty_scripts)
    print 'All illegal predicates found.'
    try:
        p = SafePopen(['find', '/', '-name', '"a"'], stdout=PIPE, timeout=0.5).safe_run()
    except:
        print 'Timeout exceeded.'