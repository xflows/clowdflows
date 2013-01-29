'''
MySQL connectivity library.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
import mysql.connector as sql
from context import DBConnection, DBContext
from ilp_db_context import RSD_DBContext

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
    rsd = RSD_DBContext(input_dict['context'])
    return {'examples' : rsd.all_examples(), 'bk' : rsd.background_knowledge()}