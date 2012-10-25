'''
Subgroup discovery interaction views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render

def build_subgroups(request, input_dict, output_dict, widget):
    data = input_dict['data']
    # Algorithms and the needed parameters
    algorithms = {
        'SD' : ['min_sup', 'g', 'beam', 'num_of_sg'],
        'SD-Preprocess' : ['min_sup', 'g', 'beam', 'num_of_sg'],
        'Apriori-SD' : ['min_sup', 'min_conf', 'k', 'num_of_sg'],
        'CN2-SD' : ['k', 'num_of_sg']
    }
    className = data.domain.class_var.name
    classValues = data.domain.class_var.values
    return render(request, 'interactions/build_subgroups.html', {'algorithms' : algorithms, 'className' : className, 'classValues' : classValues})