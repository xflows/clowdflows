# helperji, context stvari
from django.shortcuts import render, get_object_or_404, redirect
from django.http import Http404, HttpResponse
from django.contrib import messages

# auth fore
from django.contrib.auth.decorators import login_required
from django.contrib.auth import authenticate, login, logout

from workflows.models import Workflow

#settings
from mothra.settings import DEBUG, PROJECT_FOLDER

#ostalo
from django.template.loader import get_template
from django.template import TemplateDoesNotExist
import os

def index(request):
    return render(request, 'website/index.html')
    
def workflow_information(request,workflow_id):
    w = Workflow.objects.get(pk=workflow_id)
    if not w.public:
        raise Http404
    min_x = 10000
    min_y = 10000
    max_x = 0
    max_y = 0
    max_width = 300
    max_height = 200
    normalized_values = {}
    w.normalized_widgets = w.widgets.all()
    w.unique_connections = []
    w.pairs = []
    for widget in w.normalized_widgets:
        if widget.x > max_x:
            max_x = widget.x
        if widget.x < min_x:
            min_x = widget.x
        if widget.y > max_y:
            max_y = widget.y
        if widget.y < min_y:
            min_y = widget.y
    for widget in w.normalized_widgets:
        x = (widget.x - min_x)*1.0
        y = (widget.y - min_y)*1.0
        normalized_max_x = max_x-min_x
        if x == 0:
            x = 1
        if y == 0:
            y = 1
        if normalized_max_x == 0:
            normalized_max_x = x*2
        normalized_max_y = max_y-min_y
        if normalized_max_y == 0:
            normalized_max_y = y*2
        widget.norm_x = (x/normalized_max_x)*max_width
        widget.norm_y = (y/normalized_max_y)*max_height
        normalized_values[widget.id]=(widget.norm_x,widget.norm_y)
    for c in w.connections.select_related("output","input").defer("output__value","input__value").all():
        if not (c.output.widget.id,c.input.widget.id) in w.pairs:
            w.pairs.append((c.output.widget.id,c.input.widget.id))
    for pair in w.pairs:
        conn = {}
        conn['x1'] = normalized_values[pair[0]][0]+40
        conn['y1'] = normalized_values[pair[0]][1]+15
        conn['x2'] = normalized_values[pair[1]][0]-10
        conn['y2'] = normalized_values[pair[1]][1]+15
        w.unique_connections.append(conn)
    return render(request, 'website/existing.html', {'workflows':[w,]})
    
def workflows(request):
    wflows = Workflow.objects.filter(public=True)
    min_x = 10000
    min_y = 10000
    max_x = 0
    max_y = 0
    max_width = 300
    max_height = 200
    normalized_values = {}
    for w in wflows:
        w.normalized_widgets = w.widgets.all()
        w.unique_connections = []
        w.pairs = []
        for widget in w.normalized_widgets:
            if widget.x > max_x:
                max_x = widget.x
            if widget.x < min_x:
                min_x = widget.x
            if widget.y > max_y:
                max_y = widget.y
            if widget.y < min_y:
                min_y = widget.y
        for widget in w.normalized_widgets:
            x = (widget.x - min_x)*1.0
            y = (widget.y - min_y)*1.0
            normalized_max_x = max_x-min_x
            if x == 0:
                x = 1
            if y == 0:
                y = 1
            if normalized_max_x == 0:
                normalized_max_x = x*2
            normalized_max_y = max_y-min_y
            if normalized_max_y == 0:
                normalized_max_y = y*2
            widget.norm_x = (x/normalized_max_x)*max_width
            widget.norm_y = (y/normalized_max_y)*max_height
            normalized_values[widget.id]=(widget.norm_x,widget.norm_y)
        for c in w.connections.select_related("output","input").defer("output__value","input__value").all():
            if not (c.output.widget.id,c.input.widget.id) in w.pairs:
                w.pairs.append((c.output.widget.id,c.input.widget.id))
        for pair in w.pairs:
            conn = {}
            conn['x1'] = normalized_values[pair[0]][0]+40
            conn['y1'] = normalized_values[pair[0]][1]+15
            conn['x2'] = normalized_values[pair[1]][0]-10
            conn['y2'] = normalized_values[pair[1]][1]+15
            w.unique_connections.append(conn)
    return render(request, 'website/existing.html', {'workflows':wflows})
