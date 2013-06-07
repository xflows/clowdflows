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

from streams.models import *

def streaming_collect_and_display_visualization(request,widget,stream):
    try:
        swd = StreamWidgetData.objects.get(stream=stream,widget=widget)
        data = swd.value
    except Exception as e:
        swd = StreamWidgetData()
        swd.stream = stream
        swd.widget = widget
        data = []
        swd.value = data
        swd.save()
    return render(request, 'streaming_vizualizations/streaming/display_tweets.html', {'tweets':swd.value})
