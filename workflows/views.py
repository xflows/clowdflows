# helperji, context stvari
from django.shortcuts import render, get_object_or_404, redirect
from django.http import Http404, HttpResponse
from django.contrib import messages
from django.core import serializers
from django.utils import simplejson
from workflows.urls import *
from workflows.helpers import *
import workflows.interaction_views
import workflows.visualization_views
import sys
import traceback
from django.views.decorators.cache import never_cache

from jsonview.decorators import json_view

# modeli
from workflows.models import *
from django.contrib.auth.models import User

from workflows.utils import *

# auth fore
from django.contrib.auth.decorators import login_required

#settings
from mothra.settings import DEBUG, FILES_FOLDER

#ostalo
import os

from workflows import module_importer
def setattr_local(name, value, package):
    setattr(sys.modules[__name__], name, value)
module_importer.import_all_packages_libs("views",setattr_local)

@login_required
def get_category(request):
    if request.is_ajax() or DEBUG:
        c = get_object_or_404(Category, pk=request.POST['category_id'])
        if (c.user==request.user):
            return render(request, 'category.html', {'category':c})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def index(request):
    try:
        if request.user.userprofile.active_workflow is None:
            return redirect('new workflow')
    except:
        return redirect('new workflow')
    categories = Category.objects.all()
    user_categories = request.user.categories.all()
    user_widgets = request.user.widgets.filter(category=None)
    return render(request, 'index.html', {'categories':categories,'user':request.user,'user_categories':user_categories,'user_widgets':user_widgets})

@login_required
def new_workflow(request):
    w = Workflow()
    w.user = request.user
    w.save()
    request.user.userprofile.active_workflow = w
    request.user.userprofile.save()
    return redirect('editor')

@login_required
def open_workflow(request,workflow_id):
    w = get_object_or_404(Workflow, pk=workflow_id)
    if w.user == request.user:
        request.user.userprofile.active_workflow = w
        request.user.userprofile.save()
    else:
        return HttpResponse(status=400)
    return redirect('editor')

@login_required
def widget_progress(request):
    w = get_object_or_404(Widget, pk=request.GET['widget_id'])
    if w.running:
        return HttpResponse(w.progress)
    else:
        if w.progress==100:
            return HttpResponse("100")
        return HttpResponse("-1")

@login_required
def add_widget(request):
    if request.is_ajax() or DEBUG:
        if request.POST.has_key("abstractwidget_id"):
            aw = get_object_or_404(AbstractWidget, pk=request.POST['abstractwidget_id'])
            workflow = get_object_or_404(Workflow, pk=request.POST['active_workflow'])
            if (workflow.user==request.user):
                w = Widget()
                w.workflow = workflow
                w.x = int(request.POST['scrollLeft'])+50
                y = int(request.POST['scrollTop'])+50
                while workflow.widgets.filter(y=y,x=w.x).count()>0:
                    y = y + 100
                w.y = y
                w.name = aw.name
                w.abstract_widget = aw
                w.type = 'regular'
                w.save()
                inputOrder = 0
                paramOrder = 0
                for i in aw.inputs.all():
                    print(i.short_name)
                    print(aw.inputs.all())
                    j = Input()
                    j.name = i.name
                    j.short_name = i.short_name
                    j.description = i.description
                    j.variable = i.variable
                    j.widget = w
                    j.required = i.required
                    j.parameter = i.parameter
                    j.value = None
                    if (i.parameter):
                        paramOrder += 1
                        j.order = paramOrder
                    else:
                        inputOrder += 1
                        j.order = inputOrder
                    if not i.multi:
                        j.value = i.default
                    j.parameter_type = i.parameter_type
                    if i.multi:
                        j.multi_id = i.id
                    j.save()
                    for k in i.options.all():
                        o = Option()
                        o.name = k.name
                        o.value = k.value
                        o.input = j
                        o.save()
                outputOrder = 0
                for i in aw.outputs.all():
                    j = Output()
                    j.name = i.name
                    j.short_name = i.short_name
                    j.description = i.description
                    j.variable = i.variable
                    j.widget = w
                    outputOrder += 1
                    j.order = outputOrder
                    j.save()
                w.defered_outputs = w.outputs.defer("value").all()
                w.defered_inputs = w.inputs.defer("value").all()
                return render(request, 'widgets.html', {'widgets':[w,]})
            else:
                return HttpResponse(status=400)
        else:
            aw = get_object_or_404(Widget, pk=request.POST['copywidget_id'])
            if aw.type == 'regular':
                workflow = get_object_or_404(Workflow, pk=request.POST['active_workflow'])
                if (workflow.user==request.user):
                    w = Widget()
                    w.workflow = workflow
                    w.x = int(request.POST['scrollLeft'])+50
                    y = int(request.POST['scrollTop'])+50
                    while workflow.widgets.filter(y=y,x=w.x).count()>0:
                        y = y + 100
                    w.y = y
                    w.name = aw.name
                    w.abstract_widget = aw.abstract_widget
                    w.type = aw.type
                    w.save()
                    for i in aw.inputs.all():
                        j = Input()
                        j.name = i.name
                        j.short_name = i.short_name
                        j.description = i.description
                        j.variable = i.variable
                        j.widget = w
                        j.required = i.required
                        j.parameter = i.parameter
                        j.parameter_type = i.parameter_type
                        j.value = i.value
                        j.multi_id = i.multi_id
                        j.save()
                        for k in i.options.all():
                            o = Option()
                            o.name = k.name
                            o.value = k.value
                            o.input = j
                            o.save()
                    for i in aw.outputs.all():
                        j = Output()
                        j.name = i.name
                        j.short_name = i.short_name
                        j.description = i.description
                        j.variable = i.variable
                        j.widget = w
                        j.save()
                    w.defered_outputs = w.outputs.defer("value").all()
                    w.defered_inputs = w.inputs.defer("value").all()
                    return render(request, 'widgets.html', {'widgets':[w,]})
            elif aw.type=='subprocess':
                workflow = get_object_or_404(Workflow, pk=request.POST['active_workflow'])
                if (workflow.user==request.user):
                    widget_conversion = {}
                    input_conversion = {}
                    output_conversion = {}
                    w = Widget()
                    w.workflow = workflow
                    w.x = int(request.POST['scrollLeft'])+50
                    y = int(request.POST['scrollTop'])+50
                    while workflow.widgets.filter(y=y,x=w.x).count()>0:
                        y = y + 100
                    w.y = y
                    w.name = aw.name
                    w.abstract_widget = aw.abstract_widget
                    w.type = aw.type
                    w.save()
                    widget_conversion[aw.pk]=w.pk
                    for i in aw.inputs.all():
                        j = Input()
                        j.name = i.name
                        j.short_name = i.short_name
                        j.description = i.description
                        j.variable = i.variable
                        j.widget = w
                        j.required = i.required
                        j.parameter = i.parameter
                        j.parameter_type = i.parameter_type
                        j.value = i.value
                        j.multi_id = i.multi_id
                        j.save()
                        input_conversion[i.pk]=j.pk
                        for k in i.options.all():
                            o = Option()
                            o.name = k.name
                            o.value = k.value
                            o.input = j
                            o.save()
                    for i in aw.outputs.all():
                        j = Output()
                        j.name = i.name
                        j.short_name = i.short_name
                        j.description = i.description
                        j.variable = i.variable
                        j.widget = w
                        j.save()
                        output_conversion[i.pk]=j.pk
                    workflows.models.copy_workflow(aw.workflow_link, request.user, widget_conversion,input_conversion,output_conversion,w)
                    w.defered_outputs = w.outputs.defer("value").all()
                    w.defered_inputs = w.inputs.defer("value").all()
                    return render(request, 'widgets.html', {'widgets':[w,]})
            else:
                return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def save_position(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            w.x = request.POST['x']
            w.y = request.POST['y']
            w.save()
            return HttpResponse("Ok!")
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def add_connection(request):
    if request.is_ajax() or DEBUG:
        deleted = -1
        added = -1
        refresh = -1
        refreshworkflow = -1
        success = False
        mimetype = 'application/javascript'
        message = ""
        previousExists=False
        i = Input.objects.defer("value").get(pk=request.POST['input_id'])
        #i = get_object_or_404(Input, pk=request.POST['input_id'])
        o = Output.objects.defer("value").get(pk=request.POST['output_id'])
        #o = get_object_or_404(Output, pk=request.POST['output_id'])
        if (i.widget.workflow==o.widget.workflow and i.widget.workflow.user == request.user):
            if Connection.objects.filter(input=i).exists():
                previousExists=True
                new_c = Connection.objects.get(input=i)
                oldOutput = Output.objects.defer("value").get(pk=new_c.output_id)
                deleted=new_c.id
            else:
                new_c = Connection()
            new_c.input = i
            new_c.output = o
            new_c.workflow = i.widget.workflow
            new_c.save()
            if not checkForCycles(i.widget,i.widget):
                if previousExists:
                    new_c.output=oldOutput
                    new_c.save()
                else:
                    new_c.delete()
                success = False
                message = "Adding this connection would result in a cycle in the workflow."
                data = simplejson.dumps({'message':message,'success':success,'deleted':deleted,'added':added,'input_id':request.POST['input_id'],'output_id':request.POST['output_id']})
                return HttpResponse(data,mimetype)
            added = new_c.id
            new_c.input.widget.unfinish()
            if deleted==-1:
                if new_c.input.multi_id != 0:
                    i = new_c.input
                    j = Input()
                    j.name = i.name
                    j.short_name = i.short_name
                    j.description = i.description
                    j.variable = i.variable
                    j.widget = i.widget
                    j.required = i.required
                    j.parameter = i.parameter
                    j.value = None
                    j.parameter_type = i.parameter_type
                    j.multi_id = i.multi_id
                    j.save()
                    refresh = i.widget.id
                    refreshworkflow = i.widget.workflow.id
            success = True
            message = "Connection added."
            data = simplejson.dumps({'message':message,'success':success,'deleted':deleted,'added':added,'input_id':request.POST['input_id'],'output_id':request.POST['output_id'],'refresh':refresh,'refreshworkflow':refreshworkflow})
            return HttpResponse(data,mimetype)
        else:
            message = "Cannot connect widgets from different workflows."
            data = simplejson.dumps({'message':message,'success':success,'deleted':deleted,'added':added,'input_id':request.POST['input_id'],'output_id':request.POST['output_id'],'refresh':refresh},)
            return HttpResponse(data,mimetype)
    else:
        return HttpResponse(status=400)

@login_required
def delete_connection(request):
    if request.is_ajax() or DEBUG:
        c = get_object_or_404(Connection, pk=request.POST['connection_id'])
        if (c.input.widget.workflow.user!=request.user):
            return HttpResponse(status=400)
        c.input.widget.unfinish()
        mimetype = 'application/javascript'
        refresh = -1
        refreshworkflow = -1
        already_deleted=False
        if c.input.multi_id != 0:
            #pogledamo kok jih je s tem idjem, ce je vec k en, tega pobrisemo
            if c.input.widget.inputs.filter(multi_id=c.input.multi_id).count()>1:
                refresh = c.input.widget.id
                refreshworkflow = c.input.widget.workflow.id
                c.input.delete()
                already_deleted=True
        if not already_deleted:
            c.delete()
        data = simplejson.dumps({'refresh':refresh,'refreshworkflow':refreshworkflow})
        return HttpResponse(data,mimetype)
    else:
        return HttpResponse(status=400)

@login_required
def delete_widget(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        w.unfinish()
        mimetype = 'application/javascript'
        refresh = []
        delete_tab = -1
        if (w.workflow.user!=request.user):
            return HttpResponse(status=400)
        connections = Connection.objects.filter(output__widget=w).filter(input__multi_id__gt=0)
        for c in connections:
            if c.input.widget.inputs.filter(multi_id=c.input.multi_id).count()>1:
                refresh.append((c.input.widget.id,c.input.widget.workflow.id))
                c.input.delete()
        if w.type=='subprocess':
            delete_tab = w.workflow_link.id
        w.delete()
        data = simplejson.dumps({'refresh':list(set(refresh)),'delete_tab':delete_tab})
        return HttpResponse(data,mimetype)
    else:
        return HttpResponse(status=400)

@login_required
def delete_workflow(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Workflow, pk=request.GET['workflow_id'])
        if (w.user!=request.user):
            return HttpResponse(status=400)
        w.delete()
        return HttpResponse("Ok")
    else:
        return HttpResponse(status=400)

@login_required
def add_subprocess(request):
    if request.is_ajax() or DEBUG:
        workflow = get_object_or_404(Workflow, pk=request.POST['active_workflow'])
        if (workflow.user==request.user):
            new_w = Workflow()
            new_w.name = "Untitled widget"
            new_w.user = request.user
            w = Widget()
            w.workflow = workflow
            w.workflow_link = new_w
            w.x = int(request.POST['scrollLeft'])+50
            y = int(request.POST['scrollTop'])+50
            while workflow.widgets.filter(y=y,x=w.x).count()>0:
                y = y + 100
            w.y=y
            w.name = "Untitled widget"
            w.type = 'subprocess'
            w.save()
            new_w.widget = w
            new_w.save()
            w.defered_outputs = w.outputs.defer("value").all()
            w.defered_inputs = w.inputs.defer("value").all()
            return render(request, 'widgets.html', {'widgets':[w,]})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def get_subprocess(request):
    if request.is_ajax() or DEBUG:
        widget = get_object_or_404(Widget, pk=request.POST['widget_id'])
        data = simplejson.dumps({'workflow_link':widget.workflow_link.pk,'workflow_name':widget.workflow_link.name})
        mimetype = 'application/javascript'
        return HttpResponse(data,mimetype)
    else:
        return HttpResponse(status=400)

@login_required
def add_for(request):
    success = False
    mimetype = 'application/javascript'
    message = ""
    if request.is_ajax() or DEBUG:
        workflow = get_object_or_404(Workflow, pk=request.POST['active_workflow'])
        if (workflow.user==request.user):
            if workflow.widget==None:
                message = 'The for widgets can only be put in a subprocess.'
                data = simplejson.dumps({'message':message,'success':success})
                return HttpResponse(data,mimetype)
            elif workflow.widgets.filter(type='for_input').count()>0:
                message = 'This subprocess already has a for loop. Try deleting it and adding it again.'
                data = simplejson.dumps({'message':message,'success':success})
                return HttpResponse(data,mimetype)
            else:
                print(workflow)
                for_input = Widget()
                for_input.workflow = workflow
                for_input.x=int(request.POST['scrollLeft'])+50
                y=int(request.POST['scrollTop'])+50
                while workflow.widgets.filter(y=y,x=for_input.x).count()>0:
                    y = y + 100
                for_input.y=y
                for_input.name = 'For input'
                for_input.type = 'for_input'
                for_input.save()
                output = Output()
                output.name = 'For input' # subproces inner input
                output.short_name = 'for'
                output.variable = 'For'
                output.widget = for_input
                output.save()
                input = Input()
                input.widget = workflow.widget
                input.name = 'For input' # subproces input
                input.short_name = 'for'
                input.variable = 'For'
                input.inner_output = output
                input.save()
                output.outer_input = input
                output.save()

                widget = Widget()
                widget.workflow = workflow
                widget.x=int(request.POST['scrollLeft'])+200
                widget.y=int(request.POST['scrollTop'])+50
                widget.name = 'For output'
                widget.type = 'for_output'
                widget.save()
                input = Input()
                input.name = 'For output'
                input.short_name = 'for'
                input.variable = 'For'
                input.widget = widget
                input.save()
                output = Output()
                output.widget = workflow.widget
                output.name = 'For output'
                output.short_name = 'for'
                output.variable = 'For'
                output.inner_input = input
                output.save()
                input.outer_output = output
                input.save()
                for_input.defered_outputs = for_input.outputs.defer("value").all()
                for_input.defered_inputs = for_input.inputs.defer("value").all()
                widget.defered_outputs = widget.outputs.defer("value").all()
                widget.defered_inputs = widget.inputs.defer("value").all()
                return render(request, 'widgets.html', {'widgets':[for_input,widget]})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def add_cv(request):
    success = False
    mimetype = 'application/javascript'
    message = ""
    if request.is_ajax() or DEBUG:
        workflow = get_object_or_404(Workflow, pk=request.POST['active_workflow'])
        if (workflow.user==request.user):
            """Check if the workflow is from the user that has send the request"""
            if workflow.widget==None:
                message = 'The for widgets can only be put in a subprocess.'
                data = simplejson.dumps({'message':message,'success':success})
                return HttpResponse(data,mimetype)
            elif workflow.widgets.filter(type='for_input').count()>0:
                message = 'This subprocess already has a for loop. Try deleting it and adding it again.'
                data = simplejson.dumps({'message':message,'success':success})
                return HttpResponse(data,mimetype)
            elif workflow.widgets.filter(type='cv_input').count()>0:
                message = 'This subprocess already has cross validation. Try deleting it and adding it again.'
                data = simplejson.dumps({'message':message,'success':success})
                return HttpResponse(data,mimetype)
            else:
                # input: data
                cv_input_data = Widget()
                cv_input_data.workflow = workflow
                cv_input_data.x=int(request.POST['scrollLeft'])+50
                y=int(request.POST['scrollTop'])+50
                while workflow.widgets.filter(y=y,x=cv_input_data.x).count()>0:
                    y = y + 100
                cv_input_data.y=y
                cv_input_data.name = 'cv input'
                cv_input_data.type = 'cv_input'
                cv_input_data.save()

                output = Output()
                output.name = 'cv input data'
                output.short_name = 'trn' # subproces inner input
                output.variable = 'CVD'
                output.widget = cv_input_data
                output.save()
                input = Input()
                input.widget = workflow.widget
                input.name = 'cv input data'
                input.short_name = 'dat'  # subproces input
                input.variable = 'CVD'
                input.inner_output = output
                input.save()
                output.outer_input = input
                output.save()

                # input: number of folds 
                cv_input_fold = Widget()
                cv_input_fold.workflow = workflow
                cv_input_fold.x=int(request.POST['scrollLeft'])+50
                y=int(request.POST['scrollTop'])+50
                while workflow.widgets.filter(y=y,x=cv_input_fold.x).count()>0:
                    y = y + 100
                cv_input_fold.y=y
                cv_input_fold.name = 'cv input2'
                cv_input_fold.type = 'cv_input2'
                cv_input_fold.save()

                output = Output()
                output.name = 'cv input data'
                output.short_name = 'tst' # subproces inner input
                output.variable = 'CVF'
                output.widget = cv_input_data
                output.save()
                input = Input()
                input.widget = workflow.widget
                input.name = 'cv input folds'
                input.short_name = 'cvf'
                input.variable = 'CVF'
                input.inner_output = output
                input.save()
                output.outer_input = input
                output.save()

                # input: seed
                cv_input_fold = Widget()
                cv_input_fold.workflow = workflow
                cv_input_fold.x=int(request.POST['scrollLeft'])+50
                y=int(request.POST['scrollTop'])+50
                while workflow.widgets.filter(y=y,x=cv_input_fold.x).count()>0:
                    y = y + 100
                cv_input_fold.y=y
                cv_input_fold.name = 'cv input3'
                cv_input_fold.type = 'cv_input3'
                cv_input_fold.save()

                output = Output()
                output.name = 'cv input data'
                output.short_name = 'sed' # subproces inner input
                output.variable = 'CVS'
                output.widget = cv_input_data
                output.save()
                input = Input()
                input.widget = workflow.widget
                input.name = 'cv input seed'
                input.short_name = 'sed'
                input.variable = 'CVS'
                input.inner_output = output
                input.save()
                output.outer_input = input
                output.save()

                # output
                widget = Widget()
                widget.workflow = workflow
                widget.x=int(request.POST['scrollLeft'])+200
                widget.y=int(request.POST['scrollTop'])+50
                widget.name = 'cv output'
                widget.type = 'cv_output'
                widget.save()
                input = Input()
                input.name = 'cv output'
                input.short_name = 'res'
                input.variable = 'Res'
                input.widget = widget
                input.save()
                output = Output()
                output.widget = workflow.widget
                output.name = 'cv output'
                output.short_name = 'res'
                output.variable = 'Res'
                output.inner_input = input
                output.save()
                input.outer_output = output
                input.save()
                cv_input_data.defered_outputs = cv_input_data.outputs.defer("value").all()
                cv_input_data.defered_inputs = cv_input_data.inputs.defer("value").all()
                widget.defered_outputs = widget.outputs.defer("value").all()
                widget.defered_inputs = widget.inputs.defer("value").all()
                return render(request, 'widgets.html', {'widgets':[cv_input_data,widget]})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def add_input(request):
    success = False
    mimetype = 'application/javascript'
    message = ""
    if request.is_ajax() or DEBUG:
        workflow = get_object_or_404(Workflow, pk=request.POST['active_workflow'])
        if (workflow.user==request.user):
            if workflow.widget==None:
                message = 'The input widget can only be put in a subprocess.'
                data = simplejson.dumps({'message':message,'success':success})
                return HttpResponse(data,mimetype)
            else:
                widget = Widget()
                widget.workflow = workflow
                widget.x=int(request.POST['scrollLeft'])+50
                y = int(request.POST['scrollTop'])+50
                while workflow.widgets.filter(y=y,x=widget.x).count()>0:
                    y = y + 100
                widget.y=y
                widget.name = 'Input'
                widget.type = 'input'
                widget.save()
                output = Output()
                output.name = 'Input'
                output.short_name = 'inp'
                output.variable = 'Input'
                output.widget = widget
                output.save()
                input = Input()
                input.widget = workflow.widget
                input.name = 'Input'
                input.short_name = 'inp'
                input.variable = 'Input'
                input.inner_output = output
                input.save()
                output.outer_input = input
                output.save()
                widget.defered_outputs = widget.outputs.defer("value").all()
                widget.defered_inputs = widget.inputs.defer("value").all()
                return render(request, 'widgets.html', {'widgets':[widget,]})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def add_output(request):
    success = False
    mimetype = 'application/javascript'
    message = ""
    if request.is_ajax() or DEBUG:
        workflow = get_object_or_404(Workflow, pk=request.POST['active_workflow'])
        if (workflow.user==request.user):
            if workflow.widget==None:
                message = 'The output widget can only be put in a subprocess.'
                data = simplejson.dumps({'message':message,'success':success})
                return HttpResponse(data,mimetype)
            else:
                widget = Widget()
                widget.workflow = workflow
                widget.x=int(request.POST['scrollLeft'])+50
                y = int(request.POST['scrollTop'])+50
                while workflow.widgets.filter(y=y,x=widget.x).count()>0:
                    y = y + 100
                widget.y=y
                widget.name = 'Output'
                widget.type = 'output'
                widget.save()
                input = Input()
                input.name = 'Output'
                input.short_name = 'out'
                input.variable = 'Output'
                input.widget = widget
                input.save()
                output = Output()
                output.widget = workflow.widget
                output.name = 'Output'
                output.short_name = 'out'
                output.variable = 'Output'
                output.inner_input = input
                output.save()
                input.outer_output = output
                input.save()
                widget.defered_outputs = widget.outputs.defer("value").all()
                widget.defered_inputs = widget.inputs.defer("value").all()
                return render(request, 'widgets.html', {'widgets':[widget,]})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def synchronize_widgets(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Workflow,pk=request.POST['workflow_id'])
        if (w.user==request.user):
            widgets = w.widgets.all()
            defered_outputs = list(Output.objects.defer("value").filter(widget__workflow=w))
            defered_inputs = list(Input.objects.defer("value").filter(widget__workflow=w))
            for w in widgets:
                w.defered_outputs = filter(lambda e: e.widget_id == w.id,defered_outputs)
                w.defered_inputs = filter(lambda e: e.widget_id == w.id,defered_inputs)
            return render(request, 'widgets.html', {'widgets':widgets})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def synchronize_connections(request):
    if request.is_ajax() or DEBUG:
        mimetype = 'application/javascript'
        w = get_object_or_404(Workflow,pk=request.POST['workflow_id'])
        if (w.user==request.user):
            c = Connection.objects.filter(input__widget__workflow=w)
            data = serializers.serialize('json', c)
            return HttpResponse(data,mimetype)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def get_widget(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            w.defered_outputs = w.outputs.defer("value").all()
            w.defered_inputs = w.inputs.defer("value").all()
            return render(request, 'widgets.html', {'widgets':[w,]})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def get_parameters(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            return render(request, 'parameters.html', {'widget':w,'parameters':w.inputs.filter(parameter=True)})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def save_parameter(request):
    if request.is_ajax() or DEBUG:
        input = get_object_or_404(Input, pk=request.POST['input_id'])
        if (input.widget.workflow.user==request.user):
            input.value = request.POST['value']
            input.save()
            input.widget.unfinish()
            return HttpResponse(status=200)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def get_configuration(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            inputs = w.inputs.filter(parameter=False);
            parameters = w.inputs.filter(parameter=True);
            outputs = w.outputs.all();
            benchmark_exists = False
            for o in outputs:
                if o.variable=='clowdflows_elapsed':
                    benchmark_exists = True
            return render(request, 'configuration.html', {'widget':w,'inputs': inputs, 'parameters':parameters, 'outputs':outputs, 'benchmark_exists':benchmark_exists})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def save_configuration(request):
    if request.is_ajax() or DEBUG:
        widget = get_object_or_404(Widget, pk=request.POST['widgetId'])
        if (widget.workflow.user==request.user):
            inputs =  request.POST.getlist('inputs')
            params =  request.POST.getlist('params')
            outputs = request.POST.getlist('outputs')
            changed = False
            reordered = False
            deletedConnections = []
            for (id, input) in enumerate(inputs):
                inp = get_object_or_404(Input, pk=input)
                id += 1
                if (inp.widget.workflow.user!=request.user):
                    return HttpResponse(status=400)
                if (inp.parameter):
                    inp.parameter = False
                    changed = True
                    inp.save()
                if (inp.order != id):
                    inp.order = id
                    reordered = True
                    inp.save()
            for (id, input) in enumerate(params):
                inp = get_object_or_404(Input, pk=input)
                id += 1
                if (inp.widget.workflow.user!=request.user):
                    return HttpResponse(status=400)
                if (not inp.parameter):
                    #need to be careful if connections are set up to this input and need to be removed
                    for c in Connection.objects.filter(input=inp):
                        deletedConnections.append(c.id)
                        c.delete()
                    inp.parameter = True
                    changed = True
                    inp.save()
                if (inp.order != id):
                    inp.order = id
                    reordered = True
                    inp.save()
            for (id, output) in enumerate(outputs):
                out = get_object_or_404(Output, pk = output)
                id += 1
                if (out.widget.workflow.user!=request.user):
                    return HttpResponse(status=400)
                if (out.order != id):
                    out.order = id
                    reordered = True
                    out.save()
            if request.POST.get('benchmark')=='true':
                if widget.outputs.filter(variable='clowdflows_elapsed').count()==0:
                    new_o = Output()
                    new_o.widget = widget
                    new_o.variable = 'clowdflows_elapsed'
                    new_o.name = 'Elapsed time'
                    new_o.short_name = 'bmk'
                    new_o.order=99
                    new_o.save()
                changed = True
                reordered = True
            if request.POST.get('benchmark')=='false':
                o = widget.outputs.filter(variable='clowdflows_elapsed')
                if len(o)>0:
                    o.delete()
                changed = True
                reordered = True
            if (changed):
                widget.unfinish()

            data = simplejson.dumps({'changed':changed,'reordered':reordered, 'deletedConnections':deletedConnections})
            mimetype = 'application/javascript'
            return HttpResponse(data, mimetype)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def save_designation(request):
    if request.is_ajax() or DEBUG:
        for key in request.POST:
            input = get_object_or_404(AbstractInput, pk=key)
            if request.POST[key] == "input":
                input.parameter=False
                input.save()
            elif request.POST[key] == "parameter":
                input.parameter=True
                if input.parameter_type!="checkbox":
                    input.parameter_type="text"
                input.save()
        return HttpResponse(status=200)
    else:
        return HttpResponse(status=400)

@login_required
def get_parameters(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            return render(request, 'parameters.html', {'widget':w,'parameters':w.inputs.filter(parameter=True)})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def get_rename_dialog(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            return render(request, 'rename.html', {'widget':w})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def rename_widget(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            w.rename(request.POST['new_name'])
            mimetype = 'application/javascript'
            workflow_link = True
            workflow_link_id = 0
            rename_inputs = []
            rename_outputs = []
            if w.type=='input':
                rename_inputs.append(w.outputs.all()[0].outer_input.id)
                rename_outputs.append(w.outputs.all()[0].id)
            if w.type=='output':
                rename_outputs.append(w.inputs.all()[0].outer_output.id)
                rename_inputs.append(w.inputs.all()[0].id)
            try:
                workflow_link_id = w.workflow_link.id
            except Workflow.DoesNotExist:
                workflow_link = False
            data = simplejson.dumps({'workflow_link':workflow_link,'workflow_link_id':workflow_link_id,'rename_inputs':rename_inputs,'rename_outputs':rename_outputs})
            return HttpResponse(data,mimetype)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def rename_workflow(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Workflow, pk=request.POST['workflow_id'])
        if (w.user==request.user):
            w.rename(request.POST['new_name'])
            w.description = request.POST['description']
            if request.POST['public']=="true":
                w.public=True
            else:
                w.public=False
            w.save()
            mimetype = 'application/javascript'
            data = simplejson.dumps({'workflow_id':w.id})
            return HttpResponse(data,mimetype)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def run_widget(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            try:
                # find all required inputs
                for inp in w.inputs.filter(required=True,parameter=False):
                    if inp.connections.count()==0:
                        raise Exception("The input "+str(inp)+" must have something connected to it in order to run.")
                if w.type == 'for_input' or w.type == 'for_output':
                    raise Exception("You can't run for loops like this. Please run the containing widget.")
                output_dict = w.run(False)
                mimetype = 'application/javascript'
                if not w.abstract_widget is None:
                    if w.abstract_widget.interactive:
                        w.interaction_waiting = True
                        w.save()
                        data = simplejson.dumps({'status':'interactive','message':'Widget '+w.name+' needs your attention.','widget_id':w.id})
                    elif w.abstract_widget.visualization_view!='':
                        data = simplejson.dumps({'status':'visualize','message':'Visualizing widget '+w.name+'.','widget_id':w.id})
                    else:
                        data = simplejson.dumps({'status':'ok','message':'Widget '+w.name+' executed successfully.'})
                else:
                    data = simplejson.dumps({'status':'ok','message':'Widget '+w.name+' executed successfully.'})
            except Exception,e:
                mimetype = 'application/javascript'
                w.error = True
                w.running = False
                w.finished = False
                w.save()

                print traceback.format_exc(e)

                #raise
                for o in w.outputs.all():
                    o.value=None
                    o.save()
                data = simplejson.dumps({'status':'error','message':'Error occurred when trying to execute widget '+w.name+': '+str(type(e))+' '+str(e)})
            return HttpResponse(data,mimetype)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def run_tree(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            try:
                if w.type == 'for_input' or w.type == 'for_output':
                    raise Exception("You can't run for loops like this. Please run the containing widget.")
                output_dict = w.run(False)
                mimetype = 'application/javascript'
                if not w.abstract_widget is None:
                    if w.abstract_widget.interactive:
                        w.interaction_waiting = True
                        w.save()
                        data = simplejson.dumps({'status':'interactive','message':'Widget '+w.name+' needs your attention.','widget_id':w.id})
                    elif w.abstract_widget.visualization_view!='':
                        data = simplejson.dumps({'status':'visualize','message':'Visualizing widget '+w.name+'.','widget_id':w.id})
                    else:
                        data = simplejson.dumps({'status':'ok','message':'Widget '+w.name+' executed successfully.'})
                else:
                    data = simplejson.dumps({'status':'ok','message':'Widget '+w.name+' executed successfully.'})
            except Exception, e:
                mimetype = 'application/javascript'
                w.error = True
                w.running = False
                w.finished = False
                w.save()
                print traceback.format_exc(e)
                #raise
                for o in w.outputs.all():
                    o.value=None
                    o.save()
                data = simplejson.dumps({'status':'error','message':'Error occurred when trying to execute widget '+w.name+': '+str(type(e))+' '+str(e)})
            return HttpResponse(data,mimetype)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)


@login_required
def reset_widget(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            try:
                w.reset_descendants()
                data = simplejson.dumps({'status':'ok','message':'Widget '+w.name+' reset successfully.'})
                mimetype = 'application/javascript'
            except Exception, e:
                mimetype = 'application/javascript'
                w.error = True
                w.running = False
                w.finished = False
                w.save()
                print traceback.format_exc(e)

                #raise
                for o in w.outputs.all():
                    o.value=None
                    o.save()

                data = simplejson.dumps({'status':'error','message':'Error occurred when trying to reset the widget '+w.name+': '+str(type(e))+' '+str(e)})
            return HttpResponse(data,mimetype)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def reset_workflow(request):
    if request.is_ajax() or DEBUG:
        workflow = get_object_or_404(Workflow, pk=request.POST['workflow_id'])
        resetWidget = []
        for w in workflow.widgets.all():
            w.reset(False)
            resetWidget.append(w.pk)
        mimetype = 'application/javascript'
        data = simplejson.dumps({'resetWidget':resetWidget})
        return HttpResponse(data,mimetype)
    else:
        return HttpResponse(status=400)

@login_required
def get_executed_status(request):
    if request.is_ajax() or DEBUG:
        workflow = get_object_or_404(Workflow, pk=request.POST['workflow_id'])
        executedStatus = {}
        for w in workflow.widgets.all():
            if w.error or w.running or not w.finished:
                executedStatus[w.pk] = False
            else:
                executedStatus[w.pk] = True
        mimetype = 'application/javascript'
        data = simplejson.dumps({'executedStatus':executedStatus})
        return HttpResponse(data,mimetype)
    else:
        return HttpResponse(status=400)

@login_required
def visualize_widget(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            output_dict = {}
            for o in w.outputs.all():
                output_dict[o.variable]=o.value
            input_dict = {}
            for i in w.inputs.all():
                if not i.parameter:
                    if i.connections.count() > 0:
                        i.value = i.connections.all()[0].output.value
                        i.save()
                    else:
                        i.value = None
                        i.save()
                if i.multi_id == 0:
                    input_dict[i.variable]=i.value
                else:
                    if not i.variable in input_dict:
                        input_dict[i.variable]=[]
                    if not i.value == None:
                        input_dict[i.variable].append(i.value)
            view_to_call = getattr(workflows.visualization_views,w.abstract_widget.visualization_view)
            return view_to_call(request, input_dict, output_dict, w)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def widget_results(request):
    def cap(s):
        """
        Caps the display size of long strings.
        """
        if type(s) in [unicode, str] and len(s) > 300:
            return s[:300] + '\n...\''
        return s
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            import pprint
            input_dict = {}
            output_dict = {}
            for i in w.inputs.all():
                if not i.parameter:
                    if i.connections.count() > 0:
                        i.value = i.connections.all()[0].output.value
                        i.save()
                    else:
                        i.value = None
                        i.save()
                if i.multi_id == 0:
                    input_dict[i.variable] = cap(pprint.pformat(i.value))
                else:
                    if not i.variable in input_dict:
                        input_dict[i.variable]=[]
                    if not i.value == None:
                        input_dict[i.variable].append(cap(pprint.pformat(i.value)))
            for o in w.outputs.all():
                output_dict[o.variable] = cap(pprint.pformat(o.value))
            return render(request, 'visualizations/result_viewer.html', {'widget':w,'input_dict':input_dict,'output_dict':output_dict})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def documentation(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            return render(request, 'visualizations/documentation.html', {'widget':w})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def get_designate_dialogs(request):
    if request.is_ajax() or DEBUG:
        c = get_object_or_404(Category, pk=request.POST['category_id'])
        if (c.user==request.user):
            return render(request, 'designation.html', {'category':c})
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def get_unfinished(request):
    #iscemo vse unfinished katerih predhodniki so finished
    if request.is_ajax() or DEBUG:
        workflow = get_object_or_404(Workflow, pk=request.POST['workflow_id'])
        if (workflow.user==request.user):
            unfinished_list = workflow.get_ready_to_run()
            mimetype = 'application/javascript'
            data = simplejson.dumps({'ready_to_run':unfinished_list})
            return HttpResponse(data,mimetype)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)


@login_required
def unfinish_visualizations(request):
    if request.is_ajax() or DEBUG:
        workflow = get_object_or_404(Workflow, pk=request.POST['workflow_id'])
        unfinished_list = []
        for w in workflow.widgets.all():
            if w.is_visualization():
                w.unfinish()
                #w.save()
                unfinished_list.append(w.pk)
        mimetype = 'application/javascript'
        data = simplejson.dumps({'unfinished':unfinished_list})
        return HttpResponse(data,mimetype)
    else:
        return HttpResponse(status=400)

@login_required
def upload_handler(request):
    input = get_object_or_404(Input, pk=request.POST['input_id'])
    if (input.widget.workflow.user==request.user):
        destination = FILES_FOLDER+str(input.widget.workflow.id)+'/'+request.FILES['file'].name
        ensure_dir(destination)
        destination_file = open(destination, 'wb')
        for chunk in request.FILES['file'].chunks():
            destination_file.write(chunk)
        destination_file.close()
        input.value = destination
        input.save()
        input.widget.unfinish()
        error = None
        return render(request,'upload_handler.html', {"error":error,"input_id":input.id})
    else:
        return HttpResponse(status=400)

@login_required
def widget_interaction(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            input_dict = {}
            output_dict = {}
            for i in w.inputs.all():
                if not i.parameter:
                    if i.connections.count() > 0:
                        i.value = i.connections.all()[0].output.value
                        i.save()
                    else:
                        i.value = None
                        i.save()
                if i.multi_id == 0:
                    input_dict[i.variable]=i.value
                else:
                    if not i.variable in input_dict:
                        input_dict[i.variable]=[]
                    if not i.value == None:
                        input_dict[i.variable].append(i.value)
            for o in w.outputs.all():
                output_dict[o.variable]=o.value
            view_to_call = getattr(workflows.interaction_views,w.abstract_widget.interaction_view)
            return view_to_call(request, input_dict, output_dict, w)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)

@login_required
def finish_interaction(request):
    if request.is_ajax() or DEBUG:
        w = get_object_or_404(Widget, pk=request.POST['widget_id'])
        if (w.workflow.user==request.user):
            try:
                output_dict = w.run_post(dict(request.POST))
                w.interaction_waiting = False
                w.save()
                mimetype = 'application/javascript'
                data = simplejson.dumps({'status':'ok','message':'Widget '+w.name+' executed successfully.','widget_id':w.id})
            except Exception,e:
                mimetype = 'application/javascript'
                w.error = True
                w.running = False
                w.finished = False
                w.interaction_waiting = False
                w.save()
                print traceback.format_exc(e)
                #raise
                for o in w.outputs.all():
                    o.value=None
                    o.save()
                data = simplejson.dumps({'status':'error','message':'Error occurred when trying to execute widget '+w.name+': '+str(type(e))+' '+str(e),'widget_id':w.id})
            return HttpResponse(data,mimetype)
        else:
            return HttpResponse(status=400)
    else:
        return HttpResponse(status=400)


@login_required
def import_webservice(request):
    from services.webservice import WebService
    ws = WebService(request.POST['wsdl'])
    new_c = Category()
    current_name = ws.name
    i=0
    while request.user.categories.filter(name=current_name).count()>0:
        i = i + 1
        current_name = ws.name+' ('+str(i)+')'
    new_c.name=current_name
    new_c.user=request.user
    new_c.workflow=request.user.userprofile.active_workflow
    new_c.save()
    for m in ws.methods:
        new_a = AbstractWidget()
        new_a.name=m['name']
        new_a.action='call_webservice'
        new_a.wsdl=ws.wsdl_url
        new_a.wsdl_method=m['name']
        new_a.description=m['documentation']
        new_a.user=request.user
        new_a.category=new_c
        new_a.save()
        new_i = AbstractInput()
        new_i.parameter=True
        new_i.widget = new_a
        new_i.name = "Timeout"
        new_i.short_name = "to"
        new_i.variable = "timeout"
        new_i.default = '60'
        new_i.parameter_type='text'
        new_i.save()
        new_i = AbstractInput()
        new_i.parameter=True
        new_i.widget = new_a
        new_i.name = "Send empty strings to webservices"
        new_i.short_name = "ses"
        new_i.variable = "sendemptystrings"
        new_i.default = ''
        new_i.parameter_type='checkbox'
        new_i.save()
        for i in m['inputs']:
            new_i = AbstractInput()
            new_i.name = i['name']
            new_i.variable = i['name']
            new_i.short_name = i['name'][:3]
            new_i.description = ''
            new_i.required = False
            new_i.parameter= False
            if i['type']==bool:
                new_i.parameter_type='checkbox'
            new_i.default = ''
            new_i.widget = new_a
            new_i.save()
        for o in m['outputs']:
            new_o = AbstractOutput()
            new_o.name = o['name']
            new_o.variable = o['name']
            new_o.short_name = o['name'][:3]
            new_o.description = ''
            new_o.widget = new_a
            new_o.save()
    mimetype = 'application/javascript'
    data = simplejson.dumps({'category_id':new_c.id})
    return HttpResponse(data,mimetype)

@login_required
def copy_workflow(request,workflow_id):
    w = get_object_or_404(Workflow, pk=workflow_id)
    if w.user == request.user or w.public:
        new_w = workflows.models.copy_workflow(w,request.user)
        request.user.userprofile.active_workflow = new_w
        request.user.userprofile.save()
    else:
        return HttpResponse(status=400)
    return redirect('editor')

@login_required
def workflow_url(request):
    if request.is_ajax() or DEBUG:
        if request.user.userprofile.active_workflow is None:
            return HttpResponse(status=200)
        return render(request,'workflow_url.html', {"workflow":request.user.userprofile.active_workflow})
    else:
        return HttpResponse(status=200)

@login_required
def export_package(request, packages):
    try:
        if not request.user.is_staff:
            return HttpResponse(status=405)
    except:
        return HttpResponse(status=400)
    newuid = (request.GET.get('newuid', 'False').lower()=='true' or request.GET.get('n', 'False').lower()=='true')
    updateuid = (request.GET.get('updateuid', 'False').lower()=='true' or request.GET.get('u', 'False').lower()=='true')
    all = (request.GET.get('all', 'False').lower()=='true' or request.GET.get('a', 'False').lower()=='true')
    try:
        verbosity = int(request.GET.get('v', '1'))
        if verbosity == 1:
            verbosity = int(request.GET.get('verbosity', '1'))
    except:
        verbosity = 1
    packagesArray = tuple(packages.split('-'))

    class OutWriter:
        msgs = ""
        def write(self, msg):
            self.msgs += msg

    ov = OutWriter()

    from workflows.management.commands.export_package import export_package_string
    result = export_package_string(ov.write, packagesArray, newuid, updateuid, all, verbosity)
    content = '----------------------------------------\n' + \
              'Export procedure message:' +\
              "\n----------------------------------------\n" +\
              ov.msgs + \
              "\n----------------------------------------\n" + \
              "Exported data:" +\
              "\n----------------------------------------\n" +\
              result + \
              "\n----------------------------------------"

    response = HttpResponse(mimetype='text/plain',content=content)
    return response

@json_view
def widget_inputs(request, widget_id):
    w = get_object_or_404(Widget, pk=widget_id)
    input_dict = {}
    for i in w.inputs.all():
        if not i.parameter:
            if i.connections.count() > 0:
                i.value = i.connections.all()[0].output.value
                #i.save()
            else:
                i.value = None
                #i.save()
        if i.multi_id == 0:
            input_dict[i.variable]=i.value
        else:
            if not i.variable in input_dict:
                input_dict[i.variable]=[]
            if not i.value == None:
                input_dict[i.variable].append(i.value)
    return input_dict

@never_cache
def workflow_results(request,workflow_id):
    from workflows.tasks import runTest
    w = get_object_or_404(Workflow, pk=workflow_id)
    return_string = request.GET.get('result')
    s = Stream()
    a = runTest.delay(return_string)
    r = a.wait()
    #return s.execute(workflow=w)
    return HttpResponse(str(r))

@login_required
def widget_iframe(request, widget_id):
    w = get_object_or_404(Widget, pk=widget_id)
    if (w.workflow.user==request.user):
        output_dict = {}
        for o in w.outputs.all():
            output_dict[o.variable]=o.value
        input_dict = {}
        for i in w.inputs.all():
            if not i.parameter:
                if i.connections.count() > 0:
                    i.value = i.connections.all()[0].output.value
                    i.save()
                else:
                    i.value = None
                    i.save()
            if i.multi_id == 0:
                input_dict[i.variable]=i.value
            else:
                if not i.variable in input_dict:
                    input_dict[i.variable]=[]
                if not i.value == None:
                    input_dict[i.variable].append(i.value)
        view_to_call = getattr(workflows.visualization_views,w.abstract_widget.visualization_view)
        return view_to_call(request, input_dict, output_dict, w, True)
    else:
        return HttpResponse(status=400)    
    return HttpResponse("OK")