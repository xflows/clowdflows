'''
MySQL interaction views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render
from context import DBContext

def mysql_db_context(request, input_dict, output_dict, widget):
    con = input_dict['connection']
    initial_context = DBContext(con)
    return render(request, 'interactions/db_context.html', {'widget':widget, 'context': initial_context})
