'''
MySQL connectivity library.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
import mysql.connector as sql
from context import DBConnection, DBContext
from converters import RSD_Converter, Aleph_Converter, Orange_Converter

def mysql_connect(input_dict):
    user = str(input_dict['user'])
    password = str(input_dict['password'])
    host = str(input_dict['host'])
    db = str(input_dict['database'])
    con = DBConnection(user, password, host, db)
    return {'connection' : con}

def mysql_db_context(input_dict):
    return {'context' : None}

def mysql_db_context_finished(postdata, input_dict, output_dict):
    con = input_dict['connection']
    context = DBContext(con)
    context.update(postdata)
    return {'context' : context}

def mysql_rsd_converter(input_dict):
    rsd = RSD_Converter(input_dict['context'])
    return {'examples' : rsd.all_examples(), 'bk' : rsd.background_knowledge()}

def mysql_aleph_converter(input_dict):
    aleph = Aleph_Converter(input_dict['context'])
    return {'pos_examples' : aleph.positive_examples(), 'neg_examples' : aleph.negative_examples(), 'bk' : aleph.background_knowledge()}

def mysql_query_to_odt(input_dict):
    return {'dataset' : None}

def mysql_orange_converter(input_dict):
    context = input_dict['context']
    orange = Orange_Converter(context)
    return {'dataset' : orange.target_table()}
