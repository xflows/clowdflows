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

import operator

from django.core.paginator import Paginator, EmptyPage, PageNotAnInteger

def streaming_tweet_cloud(request,widget,stream):
    try:
        tweet_data = StreamWidgetData.objects.get(widget=widget,stream=stream).value
    except:
        tweet_data = []
    tweets = tweet_data

    if request.GET.get('raw_tweets')=='1':
        return render(request, 'streaming_vizualizations/streaming/raw_tweets.html', {'tweets':tweets,'widget':widget,
        'stream':stream})
    else:
        return render(request, 'streaming_vizualizations/streaming/tweet_cloud.html', {'tweets':tweets,'widget':widget,
        'stream':stream})

def streaming_display_tweets_visualization(request,widget,stream):
    try:
        tweet_data = StreamWidgetData.objects.get(widget=widget,stream=stream).value
    except:
        tweet_data = []
    tweets_unsorted = tweet_data
    tweets = sorted(tweets_unsorted, key=operator.itemgetter('created_at'))
    tweets.reverse()
    paginator = Paginator(tweets,20)
    page=request.GET.get('page')
    try:
        tweets = paginator.page(page)
    except PageNotAnInteger:
        tweets = paginator.page(1)
    except EmptyPage:
        tweets = paginator.page(paginator.num_pages)

    return render(request, 'streaming_vizualizations/streaming/display_tweets.html', {'tweets':tweets,'widget':widget,
        'stream':stream,'paged':tweets})

def streaming_collect_and_display_visualization(request,widget,stream):
    tweet_data = StreamWidgetData.objects.filter(widget=widget,stream=stream)
    tweets_unsorted = [x.value for x in tweet_data]
    tweets = sorted(tweets_unsorted, key=operator.itemgetter('created_at'))
    tweets.reverse()
    paginator = Paginator(tweets,20)
    page=request.GET.get('page')
    try:
        tweets = paginator.page(page)
    except PageNotAnInteger:
        tweets = paginator.page(1)
    except EmptyPage:
        tweets = paginator.page(paginator.num_pages)

    return render(request, 'streaming_vizualizations/streaming/display_tweets.html', {'tweets':tweets,'widget':widget,
        'stream':stream,'paged':tweets})

def streaming_sentiment_graph(request,widget,stream):
    zoomlevel = "day"
    if request.GET.has_key('zoomlevel'):
        zoomlevel = request.GET.get('zoomlevel')
    if zoomlevel == "day":
        tweet_data = StreamWidgetData.objects.filter(widget=widget,stream=stream)
        data = [x.value for x in tweet_data]
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
    if zoomlevel == "hour":
        import datetime
        tweet_data = StreamWidgetData.objects.filter(widget=widget,stream=stream)
        data = [x.value for x in tweet_data]
        aggregated_data = {}
        positive = {}
        negative = {}
        difference = {}
        for tweet in data:
            d = tweet['created_at']

            if aggregated_data.has_key(datetime.datetime(d.year,d.month,d.day,d.hour)):
                aggregated_data[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),aggregated_data[datetime.datetime(d.year,d.month,d.day,d.hour)][1]+1)
            else:
                positive[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),0)
                negative[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),0)
                difference[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),0)
                aggregated_data[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),1)
            if tweet['reliability'] != -1.0:
                if tweet['sentiment'] == "Positive":
                    positive[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),positive[datetime.datetime(d.year,d.month,d.day,d.hour)][1]+1)
                    difference[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),difference[datetime.datetime(d.year,d.month,d.day,d.hour)][1]+1)
                if tweet['sentiment'] == "Negative":
                    negative[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),negative[datetime.datetime(d.year,d.month,d.day,d.hour)][1]+1)
                    difference[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),difference[datetime.datetime(d.year,d.month,d.day,d.hour)][1]-1)
        volumes = aggregated_data.values()
        volumes.sort()
        positive = positive.values()
        positive.sort()
        negative = negative.values()
        negative.sort()
        difference = difference.values()
        difference.sort()
    if zoomlevel == "minute":
        import datetime
        tweet_data = StreamWidgetData.objects.filter(widget=widget,stream=stream)
        data = [x.value for x in tweet_data]
        aggregated_data = {}
        positive = {}
        negative = {}
        difference = {}
        for tweet in data:
            d = tweet['created_at']

            if aggregated_data.has_key(datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)):
                aggregated_data[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),aggregated_data[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)][1]+1)
            else:
                positive[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),0)
                negative[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),0)
                difference[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),0)
                aggregated_data[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),1)
            if tweet['reliability'] != -1.0:
                if tweet['sentiment'] == "Positive":
                    positive[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),positive[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)][1]+1)
                    difference[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),difference[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)][1]+1)
                if tweet['sentiment'] == "Negative":
                    negative[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),negative[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)][1]+1)
                    difference[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),difference[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)][1]-1)
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
        'tweets':data,
        'volumes':volumes,
        'positive':positive,
        'negative':negative,
        'difference':difference,
        'zoomlevel':zoomlevel,
        })
