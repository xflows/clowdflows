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

def streaming_active_annotation(request,widget,stream):
    import pickle
    from pysimplesoap.client import SoapClient, SoapFault
    import pysimplesoap
    client = SoapClient(location = "http://95.87.154.167:8088/",action = 'http://batman.ijs.si:8008/',namespace = "http://example.com/tweetsentiment.wsdl",soap_ns='soap',trace = False,ns = False)
    pysimplesoap.client.TIMEOUT = 600
    tweets = []
    pickled = pickle.dumps(str(widget.id))
    response = client.ActiveGetTweets(workflowid=pickled)
    returned_tweets = pickle.loads(str(response.ActiveGetTweetsResult))
    for tw in returned_tweets:
        if tw!='':
            tweets.append(tw)
    tweets_available = False
    if len(tweets)>0:
        tweets_available = True
    return render(request, 'streaming_vizualizations/streaming/active_learning.html', {'tweets':tweets,'widget':widget,'stream':stream,'tweets_available':tweets_available})

def streaming_active_annotation2(request,widget,stream):
    import pickle
    from pysimplesoap.client import SoapClient, SoapFault
    import pysimplesoap
    client = SoapClient(location = "http://95.87.154.167:8098/",action = 'http://95.87.154.167:8098/',namespace = "http://example.com/tweetsentiment.wsdl",soap_ns='soap',trace = False,ns = False)
    pysimplesoap.client.TIMEOUT = 600
    tweets = []
    #here we get all the input_dict data
    strategycount = "8"
    randomcount = "2"
    for i in widget.inputs.all():
        if i.variable == 'q_strategy_closest':
            strategycount = i.value
        if i.variable == 'q_strategy_random':
            randomcount = i.value
    pickled = pickle.dumps((str(widget.id),strategycount,randomcount))
    response = client.ActiveGetTweets(workflowid=pickled)
    returned_tweets = pickle.loads(str(response.ActiveGetTweetsResult))
    for tw in returned_tweets:
        if tw!='':
            tweets.append(tw)
    tweets_available = False
    if len(tweets)>0:
        tweets_available = True
    return render(request, 'streaming_vizualizations/streaming/active_learning.html', {'tweets':tweets,'widget':widget,'stream':stream,'tweets_available':tweets_available})


def streaming_display_tweets_visualization(request,widget,stream):
    try:
        tweet_data = StreamWidgetData.objects.get(widget=widget,stream=stream).value
    except:
        tweet_data = []
    tweets_unsorted = tweet_data
    tweets = sorted(tweets_unsorted, key=operator.itemgetter('created_at'))
    if request.GET.get('reverse')=="true":
        pass
    else:
        tweets.reverse()
    rpp=20
    if request.GET.has_key('rpp'):
        rpp = int(request.GET.get('rpp'))
        if rpp<1:
            rpp = 20
    paginator = Paginator(tweets,rpp)
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

def streaming_triplet_graph_visualization(request,widget,stream):
    try:
        triplets = StreamWidgetData.objects.get(widget=widget,stream=stream).value
    except:
        triplets = []
    return render(request, 'streaming_vizualizations/streaming/triplet_graph.html', {'triplets':triplets,'widget':widget,'stream':stream})


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
            try:
                if tweet['reliability'] != -1.0:
                    if tweet['sentiment'] == "Positive":
                        positive[tweet['created_at'].date()] = (tweet['created_at'].date(),positive[tweet['created_at'].date()][1]+1)
                        difference[tweet['created_at'].date()] = (tweet['created_at'].date(),difference[tweet['created_at'].date()][1]+1)
                    if tweet['sentiment'] == "Negative":
                        negative[tweet['created_at'].date()] = (tweet['created_at'].date(),negative[tweet['created_at'].date()][1]+1)
                        difference[tweet['created_at'].date()] = (tweet['created_at'].date(),difference[tweet['created_at'].date()][1]-1)
            except:
                pass
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
            try:                
                if tweet['reliability'] != -1.0:
                    if tweet['sentiment'] == "Positive":
                        positive[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),positive[datetime.datetime(d.year,d.month,d.day,d.hour)][1]+1)
                        difference[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),difference[datetime.datetime(d.year,d.month,d.day,d.hour)][1]+1)
                    if tweet['sentiment'] == "Negative":
                        negative[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),negative[datetime.datetime(d.year,d.month,d.day,d.hour)][1]+1)
                        difference[datetime.datetime(d.year,d.month,d.day,d.hour)] = (datetime.datetime(d.year,d.month,d.day,d.hour),difference[datetime.datetime(d.year,d.month,d.day,d.hour)][1]-1)
            except:
                pass                        
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
            try:                
                if tweet['reliability'] != -1.0:
                    if tweet['sentiment'] == "Positive":
                        positive[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),positive[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)][1]+1)
                        difference[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),difference[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)][1]+1)
                    if tweet['sentiment'] == "Negative":
                        negative[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),negative[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)][1]+1)
                        difference[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)] = (datetime.datetime(d.year,d.month,d.day,d.hour,d.minute),difference[datetime.datetime(d.year,d.month,d.day,d.hour,d.minute)][1]-1)
            except:
                pass                        
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
