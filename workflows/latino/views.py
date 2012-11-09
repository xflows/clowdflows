# helperji, context stvari
from django.shortcuts import render, get_object_or_404, redirect
from django.http import Http404, HttpResponse

# modeli
from workflows.models import *

# auth fore
from django.contrib.auth.decorators import login_required

# latino
import latino
import logging

@login_required
def get_adc_index(request, widget_id, narrow_doc = 'n', document_id_from=0, document_id_to=-1):
    logging.info('__get_adc_index__')
    w = get_object_or_404(Widget, pk=widget_id)
    if w.workflow.user == request.user:
        firstInput = w.inputs.all()[0]
        adc = firstInput.value
        return latino.makeAdcIndexPage(adc, document_id_from, document_id_to, narrow_doc=='n')
    else:
        return HttpResponse(status=400)

@login_required
def get_adc_page(request, widget_id, document_id, narrow_doc = 'n'):
    logging.info('__get_adc_page__')
    w = get_object_or_404(Widget, pk=widget_id)
    if w.workflow.user == request.user:
        firstInput = w.inputs.all()[0]
        adc = firstInput.value
        return latino.makeAdcDocPage(adc, document_id, narrow_doc)
    else:
        return HttpResponse(status=400)