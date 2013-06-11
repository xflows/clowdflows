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
    return render(request, 'streaming_vizualizations/streaming/display_tweets.html', {'tweets':swd.value,'widget':widget,
        'stream':stream})

def streaming_sentiment_graph(request,widget,stream):
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
    aggregated_data = {}
    positive = {}
    negative = {}
    difference = {}
    for tweet in data:
        if aggregated_data.has_key(tweet['created_at'].date()):
            aggregated_data[tweet['created_at'].date()] = (tweet['created_at'].date(),aggregated_data[tweet['created_at'].date()][1]+1)
        else:
            positive[tweet['created_at'].date()] = (tweet['created_at'].date(),0)
            negative[tweet['created_at'].date()] = (tweet['created_at'].date(),0)
            difference[tweet['created_at'].date()] = (tweet['created_at'].date(),0)
            aggregated_data[tweet['created_at'].date()] = (tweet['created_at'].date(),1)
        if tweet['reliability'] != -1.0:
            if tweet['sentiment'] == "Positive":
                positive[tweet['created_at'].date()] = (tweet['created_at'].date(),positive[tweet['created_at'].date()][1]+1)
                difference[tweet['created_at'].date()] = (tweet['created_at'].date(),difference[tweet['created_at'].date()][1]+1)
            if tweet['sentiment'] == "Negative":
                negative[tweet['created_at'].date()] = (tweet['created_at'].date(),negative[tweet['created_at'].date()][1]+1)
                difference[tweet['created_at'].date()] = (tweet['created_at'].date(),difference[tweet['created_at'].date()][1]-1)
    volumes = aggregated_data.values()
    volumes.sort()
    positive = positive.values()
    positive.sort()
    negative = negative.values()
    negative.sort()
    difference = difference.values()
    difference.sort()
    return render(request, 'streaming_vizualizations/streaming/sentiment_graph.html',
        {'widget':widget,
        'stream':stream,
        'tweets':swd.value,
        'volumes':volumes,
        'positive':positive,
        'negative':negative,
        'difference':difference,
        })
