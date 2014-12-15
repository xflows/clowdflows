'''
Segmine visualization viewes.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render
import os
import json


def segmine_biomine_visualizer(request, input_dict, output_dict, widget):
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir
    filename = os.path.join(str(request.user.id), str(widget.id) + '.bmg')
    destination = os.path.join(MEDIA_ROOT, filename)
    ensure_dir(destination)
    f = open(destination,'w')
    f.write(str(input_dict['graph']))
    f.close()
    return render(request, 'visualizations/segmine_biomine_visualizer.html', 
                  {'widget':widget, 'filename' : filename})


def segmine_rank_plotter(request, input_dict, output_dict, widget):
    scores = input_dict['ranks']
    scores = zip(*scores)[1]
    return render(request, 'visualizations/segmine_rank_plotter.html', 
                  {'widget': widget, 'gene_ranks': json.dumps(scores)})
