# helperji, context stvari
from django.shortcuts import render, get_object_or_404, redirect
from django.http import Http404, HttpResponse
from django.contrib import messages

# auth fore
from django.contrib.auth.decorators import login_required
from django.contrib.auth import authenticate, login, logout

from workflows.models import Workflow, Connection

from streams.models import Stream

#settings
from mothra.settings import DEBUG, PROJECT_FOLDER

#ostalo
from django.template.loader import get_template
from django.template import TemplateDoesNotExist
import os

import json

from website.forms import ImportForm

def index(request):
    return render(request, 'website/index.html')

def reset_stream(request,stream_id):
    s = get_object_or_404(Stream, pk=stream_id)
    if s.user != request.user:
        raise Http404
    s.reset()
    s.save()
    return redirect(s.get_absolute_url())

def deactivate_stream(request,stream_id):
    s = get_object_or_404(Stream, pk=stream_id)
    if s.user != request.user:
        raise Http404
    s.active = False
    s.save()
    return redirect(s.get_absolute_url())

def activate_stream(request,stream_id):
    s = get_object_or_404(Stream, pk=stream_id)
    if s.user != request.user:
        raise Http404
    s.active = True
    s.save()
    return redirect(s.get_absolute_url())

def start_stream(request,workflow_id):
    w = get_object_or_404(Workflow, pk=workflow_id)
    if w.user != request.user:
        raise Http404
    s = Stream(workflow=w,user=request.user,active=True)
    s.save()
    return redirect(s.get_absolute_url())

def stream(request,stream_id):
    stream = get_object_or_404(Stream,pk=stream_id)
    if stream.workflow.user != request.user:
        raise Http404
    return render(request, 'website/stream.html', {'stream':stream})

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
        if not (c.output.widget_id,c.input.widget_id) in w.pairs:
            w.pairs.append((c.output.widget_id,c.input.widget_id))
    for pair in w.pairs:
        conn = {}
        conn['x1'] = normalized_values[pair[0]][0]+40
        conn['y1'] = normalized_values[pair[0]][1]+15
        conn['x2'] = normalized_values[pair[1]][0]-10
        conn['y2'] = normalized_values[pair[1]][1]+15
        w.unique_connections.append(conn)
    return render(request, 'website/existing.html', {'workflows':[w,]})

@login_required
def your_workflows(request):
    return render(request, 'website/yourworkflows.html', {'workflows':request.user.workflows.filter(widget=None)})

@login_required
def make_public(request,workflow_id):
    workflow = get_object_or_404(Workflow,pk=workflow_id)
    if request.user == workflow.user:
        workflow.public = True
        workflow.save()
    return redirect('your workflows')

@login_required
def make_private(request,workflow_id):
    workflow = get_object_or_404(Workflow,pk=workflow_id)
    if request.user == workflow.user:
        workflow.public = False
        workflow.save()
    return redirect('your workflows')

def workflows(request):
    wflows = Workflow.objects.filter(public=True)
    min_x = 10000
    min_y = 10000
    max_x = 0
    max_y = 0
    max_width = 300
    max_height = 200
    normalized_values = {}
    cons = list(Connection.objects.select_related("output","input").defer("output__value","input__value").filter(workflow__public=True))
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
        connection_list = filter(lambda e: e.workflow_id == w.id, cons)
        for c in connection_list:
            if not (c.output.widget_id,c.input.widget_id) in w.pairs:
                w.pairs.append((c.output.widget_id,c.input.widget_id))
        for pair in w.pairs:
            conn = {}
            conn['x1'] = normalized_values[pair[0]][0]+40
            conn['y1'] = normalized_values[pair[0]][1]+15
            conn['x2'] = normalized_values[pair[1]][0]-10
            conn['y2'] = normalized_values[pair[1]][1]+15
            w.unique_connections.append(conn)
    return render(request, 'website/existing.html', {'workflows':wflows})

@login_required
def editor(request):
    if request.GET.get('tutorial','0')=='1':
        tutorial = True
    else:
        tutorial = False
    return render(request, 'website/editor.html', {'tutorial':tutorial})

@login_required
def export_workflow(request,workflow_id):
    w = get_object_or_404(Workflow, pk=workflow_id)
    exported_data = json.dumps(w.export(),indent=2)
    return render(request,'website/export_workflow.html',{"workflow":w,'exported_data':exported_data})

@login_required
def import_workflow(request):
    if request.method == "POST":
        form = ImportForm(request.POST)
        if form.is_valid():
            new_workflow = Workflow()
            new_workflow.user = request.user
            new_workflow.import_from_json(json.loads(form.cleaned_data['data']),{},{})
            return redirect(new_workflow.get_absolute_url())
    else:
        form = ImportForm()
    return render(request,'website/import_workflow.html',{"form":form})