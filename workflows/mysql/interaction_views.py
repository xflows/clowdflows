'''
MySQL interaction views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
import json
from django.shortcuts import render
from context import DBContext

def mysql_db_context(request, input_dict, output_dict, widget):
    con = input_dict['connection']
    initial_context = DBContext(con)
    initial_target_cols = initial_context.cols[initial_context.target_table]
    initial_target_col_vals = initial_context.col_vals[initial_target_cols[0]]
    cols_dump = json.dumps(initial_context.cols)
    col_vals_dump = json.dumps(initial_context.col_vals)
    return render(request, 'interactions/db_context.html', {'widget':widget, 'context': initial_context, 'target_cols' : initial_target_cols, 'cols' : cols_dump, 'col_vals' : col_vals_dump, 'target_col_vals' : initial_target_col_vals})
