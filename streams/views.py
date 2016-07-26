# helperji, context stvari
from django.shortcuts import render, get_object_or_404, redirect
from django.http import Http404, HttpResponse
from django.contrib import messages
from django.core import serializers
# from django.utils import simplejson
from workflows.urls import *
from workflows.helpers import *
import workflows.interaction_views
import workflows.visualization_views
import sys
import traceback

# modeli
from workflows.models import *
from django.contrib.auth.models import User

from workflows.utils import *

# auth fore
from django.contrib.auth.decorators import login_required

#settings
from mothra.settings import DEBUG, FILES_FOLDER

from streams.models import Stream

import workflows.views

#ostalo
import os

def stream_widget_visualization(request,stream_id,widget_id):
    stream = get_object_or_404(Stream,pk=stream_id)
    widget = get_object_or_404(Widget,pk=widget_id)
    if widget.abstract_widget.streaming_visualization_view == '':
        return Http404
    else:
        view_to_call = getattr(workflows.views,widget.abstract_widget.streaming_visualization_view)
        return view_to_call(request,widget,stream)

