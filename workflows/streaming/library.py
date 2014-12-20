# -*- coding: utf-8 -*-
'''
Streaming widgets librarby

@author: Janez Kranjc <janez.kranjc@ijs.si>
'''

from workflows.security import safeOpen

def streaming_add_neutral_zone(input_dict):
    import copy
    tweets = copy.deepcopy(input_dict['ltw'])
    neutral_zone = float(input_dict['zone'])

    ltw = []

    for tweet in tweets:
        if tweet['reliability']!=-1.0 and tweet['reliability']<neutral_zone:
            tweet['sentiment']="Neutral"
        ltw.append(tweet)

    output_dict = {}

    output_dict['ltw']=ltw
    return output_dict

def streaming_remove_words_from_tweets(input_dict):
    import copy
    tweets = copy.deepcopy(input_dict['ltw'])
    words = input_dict['words'].encode("utf-8")
    words = words.split("\n")

    ltw = []
    import re

    for tweet in tweets:
        for word in words:
            pattern = re.compile(word, re.IGNORECASE)
            tweet['text']=pattern.sub('',tweet['text'])
        ltw.append(tweet)

    output_dict = {}

    output_dict['ltw']=ltw
    return output_dict

def streaming_simulate_stream_from_text_file(input_dict,widget,stream=None):
    import datetime
    csvfile = safeOpen(input_dict['file'])
    tweet_data = csvfile.read()
    tweet_data = tweet_data.strip()
    tweets = tweet_data.split("\n")
    ltw = []
    i=1
    for tw in tweets:
        tweet = {}
        tweet['id']=i
        tweet['created_at']=datetime.datetime.now()
        tweet['text']=tw
        tweet['user']="dragi"
        tweet['lang']="bg"
        i=i+1
        ltw.append(tweet)
    output_dict = {}
    output_dict['ltw']=ltw
    return output_dict

def streaming_simulate_stream_from_csv(input_dict,widget,stream=None):
    from streams.models import StreamWidgetData
    import datetime
    import csv
    csvfile = safeOpen(input_dict['csv'])
    csvreader = csv.reader(csvfile,delimiter=";",quotechar='"')
    rows = []
    ltw = []
    i=0
    counter = 0
    started = False
    last_id = "not-started-yet"
    if not stream is None:
        try:
            swd = StreamWidgetData.objects.get(stream=stream,widget=widget)
            last_id = swd.value
        except:
            started = True
    else:
        started = True
    for row in csvreader:
        rows.append(row)
        if i!=0:
            rows[i][1] = datetime.datetime.strptime(rows[i][1],"%m/%d/%Y %I:%M:%S %p")
            tweet = {}
            tweet['id'] = rows[i][0]
            tweet['created_at'] = rows[i][1]
            tweet['text'] = rows[i][3].encode('utf-8')
            tweet['user'] = rows[i][5].encode('utf-8')
            tweet['lang'] = rows[i][11]
            if started:
                counter = counter + 1
                ltw.append(tweet)
            if counter == 50 and started:
                started = False
                if not stream is None:
                    try:
                        swd = StreamWidgetData.objects.get(stream=stream,widget=widget)
                        swd.value = tweet['id']
                        swd.save()
                    except:
                        swd = StreamWidgetData()
                        swd.stream = stream
                        swd.widget = widget
                        data = tweet['id']
                        swd.value = data
                        swd.save()
            if tweet['id']==last_id:
                started = True
        i = i + 1
    if counter < 51 and not stream is None and started == True:
        try:
            swd = StreamWidgetData.objects.get(stream=stream,widget=widget)
            swd.value = "done"
            swd.save()
        except:
            swd = StreamWidgetData()
            swd.stream = stream
            swd.widget = widget
            data = "done"
            swd.value = data
            swd.save()
    output_dict = {}
    #print ltw
    #print len(ltw)
    output_dict['ltw']=ltw
    return output_dict

def streaming_split_pos_neg(input_dict):

    tweets = input_dict['ltw']

    positive_tweets = []
    negative_tweets = []

    for tweet in tweets:
        try:
            if tweet['sentiment']=="Positive":
                positive_tweets.append(tweet)
            if tweet['sentiment']=="Negative" and tweet['reliability']!=-1.0:
                negative_tweets.append(tweet)
        except:
            pass

    output_dict = {}

    output_dict['ptw']=positive_tweets
    output_dict['ntw']=negative_tweets
    return output_dict

def streaming_filter_tweets_by_language(input_dict):

    language = input_dict['lang']

    tweets = input_dict['ltw']

    new_tweets = []

    for tweet in tweets:
        if tweet['lang']==language:
            new_tweets.append(tweet)

    output_dict = {}
    output_dict['ltw']=new_tweets
    return output_dict

def streaming_display_tweets(input_dict,widget,stream=None):
    from streams.models import StreamWidgetData
    if stream is None:
        return {}
    else:
        try:
            swd = StreamWidgetData.objects.get(stream=stream,widget=widget)
            swd.value = input_dict['ltw']
            swd.save()
        except:
            swd = StreamWidgetData()
            swd.stream = stream
            swd.widget = widget
            data = input_dict['ltw']
            swd.value = data
            swd.save()
        return {}

def streaming_triplet_graph(input_dict,widget,stream=None):
    from streams.models import StreamWidgetData
    if stream is None:
        return {}
    else:
        try:
            swd = StreamWidgetData.objects.get(stream=stream,widget=widget)
            swd.value = input_dict['triplets']
            swd.save()
        except:
            swd = StreamWidgetData()
            swd.stream = stream
            swd.widget = widget
            data = input_dict['triplets']
            swd.value = data
            swd.save()
        return {}

def streaming_collect_and_display_tweets(input_dict,widget,stream=None):
    from streams.models import StreamWidgetData
    if stream is None:
        return {}
    else:
        new_tweets = []
        for tweet in input_dict['ltw']:
            new_tweets.append(StreamWidgetData(stream=stream,widget=widget,value=tweet))
        StreamWidgetData.objects.bulk_create(new_tweets)
        return {}

def streaming_sentiment_graph(input_dict,widget,stream=None):
    from streams.models import StreamWidgetData
    if stream is None:
        return {}
    else:
        new_tweets = []
        for tweet in input_dict['ltw']:
            new_tweets.append(StreamWidgetData(stream=stream,widget=widget,value=tweet))
        StreamWidgetData.objects.bulk_create(new_tweets)
        return {}

def streaming_tweet_sentiment_service(input_dict,widget,stream=None):
    import pickle
    from pysimplesoap.client import SoapClient, SoapFault
    import pysimplesoap

    client = SoapClient(location = "http://95.87.154.167:8088/",action = 'http://95.87.154.167:8088/',namespace = "http://example.com/tweetsentiment.wsdl",soap_ns='soap',trace = False,ns = False)
    pysimplesoap.client.TIMEOUT = 60

    list_of_tweets = input_dict['ltw']

    new_list_of_tweets = []

    for tweet in list_of_tweets:
        new_list_of_tweets.append({'id':tweet['id'],'text':tweet['text'],'language':tweet['lang']})

    pickled_list_of_tweets = pickle.dumps(new_list_of_tweets)

    response = client.TweetSentimentService(tweets=pickled_list_of_tweets)

    new_ltw = pickle.loads(str(response.TweetSentimentResult))

    i=0
    for new_tweet in new_ltw:
        list_of_tweets[i]['sentiment']=new_tweet['sentiment']
        list_of_tweets[i]['lang']=new_tweet['language']
        list_of_tweets[i]['reliability']=new_tweet['reliability']
        i = i + 1

    output_dict = {}

    output_dict['ltw'] = list_of_tweets

    return output_dict

def streaming_triplet_porter_stemmer(input_dict,widget,stream=None):
    import nltk
    triplets = input_dict['triplets']
    new_triplets = []
    porter_stemmer = nltk.stem.PorterStemmer()
    for triplet in triplets:
        new_triplet = []
        for word in triplet:
            try:
                new_triplet.append(porter_stemmer.stem(word))
            except:
                new_triplet.append(word)
        new_triplets.append(new_triplet)
    output_dict = {}
    output_dict['triplets']=new_triplets
    return output_dict

def streaming_triplet_wordnet_lemmatizer(input_dict,widget,stream=None):
    import nltk
    triplets = input_dict['triplets']
    new_triplets = []
    lemmatizer = nltk.stem.WordNetLemmatizer()
    for triplet in triplets:
        new_triplet = []
        for word in triplet:
            try:
                new_triplet.append(lemmatizer.lemmatize(word))
            except:
                new_triplet.append(word)
        new_triplets.append(new_triplet)
    output_dict = {}
    output_dict['triplets']=new_triplets
    return output_dict    

def streaming_triplet_extraction(input_dict,widget,stream=None):
    from pysimplesoap.client import SoapClient, SoapFault
    import pysimplesoap
    import re
    client = SoapClient(location = "http://95.87.154.167:8008/",action = 'http://95.87.154.167:8008/',namespace = "http://example.com/tweetsentiment.wsdl",soap_ns='soap',trace = False,ns = False)
    pysimplesoap.client.TIMEOUT = 36000

    text = input_dict['text']
    response = client.TripletExtraction(text=text)

    triplets = str(response.TripletExtractionResult)
    triplets = triplets.split("\n")

    new_triplets = []

    for triplet in triplets:
        t = re.sub(r'\([^)]*\)', "",triplet)
        triplet = []
        words = t.strip().split(" ")
        for word in words:
            if len(word)>0:
                triplet.append(word.lower())
        if len(triplet)>0:
            new_triplets.append(triplet)


    output_dict = {}
    output_dict['triplets'] = new_triplets

    return output_dict

def streaming_summarize_url(input_dict,widget,stream=None):
    import pyteaser
    summaries = pyteaser.SummarizeUrl(input_dict['url'])
    output_dict = {}
    output_dict['summary']=" ".join(summaries)
    return output_dict

def streaming_get_article_text(input_dict,widget,stream=None):
    import pyteaser
    text = pyteaser.grab_link(input_dict['url'])
    output_dict = {}
    output_dict['text']=text.cleaned_text
    return output_dict

def streaming_active_sentiment_analysis(input_dict,widget,stream=None):
    import pickle
    from pysimplesoap.client import SoapClient, SoapFault
    import pysimplesoap
    client = SoapClient(location = "http://95.87.154.167:8088/",action = 'http://95.87.154.167:8088/',namespace = "http://example.com/tweetsentiment.wsdl",soap_ns='soap',trace = False,ns = False)
    pysimplesoap.client.TIMEOUT = 600

    list_of_tweets = input_dict['ltw']

    new_list_of_tweets = []

    for tweet in list_of_tweets:
        new_list_of_tweets.append(tweet['text'])

    workflow_id = widget.id

    service_input = pickle.dumps((str(workflow_id),new_list_of_tweets))
    #print service_input
    response = client.ActiveClassifyMultiple(workflowtweets=service_input)

    i=0
    new_ltw = pickle.loads(str(response.ActiveClassifyMultipleResult))

    for new_tweet in new_ltw:
        if new_tweet[0]=="True":
            list_of_tweets[i]['sentiment']="Positive"
        elif new_tweet[0]=="False":
            list_of_tweets[i]['sentiment']="Negative"
        list_of_tweets[i]['reliability']=new_tweet[1]
        i=i+1

    output_dict = {}

    output_dict['ltw'] = list_of_tweets

    return output_dict

def streaming_active_sentiment_analysis2(input_dict,widget,stream=None):
    import pickle
    from pysimplesoap.client import SoapClient, SoapFault
    import pysimplesoap
    client = SoapClient(location = "http://95.87.154.167:8098/",action = 'http://95.87.154.167:8098/',namespace = "http://example.com/tweetsentiment.wsdl",soap_ns='soap',trace = False,ns = False)
    pysimplesoap.client.TIMEOUT = 600

    list_of_tweets = input_dict['ltw']

    new_list_of_tweets = []

    for tweet in list_of_tweets:
        new_list_of_tweets.append(tweet['text'])

    workflow_id = widget.id

    service_input = pickle.dumps((str(workflow_id),input_dict['b_size'],input_dict['q_strategy_closest'],input_dict['q_strategy_random'],new_list_of_tweets))
    #print service_input
    response = client.ActiveClassifyMultiple(workflowtweets=service_input)

    i=0
    new_ltw = pickle.loads(str(response.ActiveClassifyMultipleResult))

    for new_tweet in new_ltw:
        if new_tweet[0]=="True":
            list_of_tweets[i]['sentiment']="Positive"
        elif new_tweet[0]=="False":
            list_of_tweets[i]['sentiment']="Negative"
        list_of_tweets[i]['reliability']=new_tweet[1]
        i=i+1

    output_dict = {}

    output_dict['ltw'] = list_of_tweets

    return output_dict

def streaming_sentiment_analysis(input_dict,widget,stream=None):
    import pickle
    from pysimplesoap.client import SoapClient, SoapFault
    import pysimplesoap

    client = SoapClient(location = "http://95.87.154.167:8088/",action = 'http://95.87.154.167:8088/',namespace = "http://example.com/tweetsentiment.wsdl",soap_ns='soap',trace = False,ns = False)
    pysimplesoap.client.TIMEOUT = 60

    try:
        input_dict['lang']
    except:
        input_dict['lang']="en"

    #list_of_tweets = input_dict['ltw']

    new_list_of_tweets = []

    new_list_of_tweets.append({'id':0,'text':input_dict['text'],'language':input_dict['lang']})

    #for tweet in list_of_tweets:
    #    new_list_of_tweets.append({'id':tweet['id'],'text':tweet['text'],'language':tweet['lang']})

    pickled_list_of_tweets = pickle.dumps(new_list_of_tweets)

    response = client.TweetSentimentService(tweets=pickled_list_of_tweets)

    new_ltw = pickle.loads(str(response.TweetSentimentResult))

    output_dict = {}

    #i=0
    for new_tweet in new_ltw:
        output_dict['sentiment']=new_tweet['sentiment']
        output_dict['language']=new_tweet['language']
        output_dict['reliability']=new_tweet['reliability']
        #list_of_tweets[i]['sentiment']=new_tweet['sentiment']
        #list_of_tweets[i]['lang']=new_tweet['language']
        #list_of_tweets[i]['reliability']=new_tweet['reliability']
        #i = i + 1

    #output_dict = {}

    #output_dict['ltw'] = list_of_tweets

    return output_dict

def streaming_twitter(input_dict,widget,stream=None):
    import tweepy
    from streams.models import StreamWidgetData
    from streams.models import HaltStream

    if input_dict['cfauth']=="true":
        consumer_key="zmK41mqxU3ZNJTFQpYwTdg"
        consumer_secret="9StnKNAe20ebDOREQjsVjAjBEiz5R9feZJTGUYWqLo"
        access_token="45210078-VydgdJMwhWYjZRvlNbrKj6jfqicUIsdMnRbnaPElL"
        access_token_secret="uLvIN3MMxFSxdK4M8P5RYojjUkbc2reqNydYtpT7Ks"
    else:
        consumer_key = input_dict['ck']
        consumer_secret = input_dict['cs']
        access_token = input_dict['at']
        access_token_secret = input_dict['as']

    auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)

    api = tweepy.API(auth)

    query = input_dict['query']

    rate_limit_status = api.rate_limit_status()

    if rate_limit_status['resources']['search']['/search/tweets']['remaining']>0:

        if stream is None:
            try:
                ltw = api.new_search(q=input_dict['query'],geocode=input_dict['geocode'],count=100)
            except Exception as e:
                raise HaltStream("The Twitter API returned an error: "+str(e))
        else:
            try:
                swd = StreamWidgetData.objects.get(stream=stream,widget=widget)
                data = swd.value
            except Exception as e:
                swd = StreamWidgetData()
                swd.stream = stream
                swd.widget = widget
                data = {}
                swd.value = data
                swd.save()
            if data.has_key(query):
                since_id = data[query]
                try:
                    ltw = api.new_search(q=input_dict['query'],geocode=input_dict['geocode'],count=100,since_id=since_id)
                except Exception as e:
                    raise HaltStream("The Twitter API returned an error: "+str(e))
            else:
                try:
                    ltw = api.new_search(q=input_dict['query'],geocode=input_dict['geocode'],count=100)
                except Exception as e:
                    raise HaltStream("The Twitter API returned an error: "+str(e))
            if len(ltw)>0:
                data[query]=ltw[0].id
                swd.value = data
                swd.save()
    else:
        import datetime
        import time
        current_time = int(time.mktime(datetime.datetime.now().timetuple()))
        remaining = rate_limit_status['resources']['search']['/search/tweets']['reset']-current_time
        if input_dict['cfauth']=="true":
            raise HaltStream("The twitter API limit has been reached. Try again in "+str(remaining)+" seconds. Try using your own credentials.")
        else:
            raise HaltStream("The twitter API limit has been reached. Try again in "+str(remaining)+" seconds.")

    output_dict = {}

    tweets = []

    for tw in ltw:
        tweet = {}
        tweet['id'] = tw.id
        tweet['created_at'] = tw.created_at
        tweet['text'] = unicode(tw.text).encode("utf-8")
        try:
            tweet['user'] = tw.user['screen_name']
        except:
            tweet['user'] = ""
        tweet['lang'] = tw.lang
        tweets.append(tweet)

    if len(tweets)>0 or stream is None:
        output_dict['ltw']=tweets
        return output_dict
    else:
        raise HaltStream("No new results, halting stream.")

def streaming_rss_reader(input_dict,widget,stream=None):
    import feedparser
    from streams.models import StreamWidgetData
    feed = feedparser.parse(input_dict['url'])
    output_dict = {}
    if stream is None:
        output_dict['url'] = feed['items'][0]['link']
    else:
        try:
            swd = StreamWidgetData.objects.get(stream=stream,widget=widget)
            data = swd.value
        except:
            swd = StreamWidgetData()
            swd.stream = stream
            swd.widget = widget
            data = []
            swd.value = data
            swd.save()
        feed_length = len(feed['items'])
        feed['items'].reverse()
        for item in feed['items']:
            if item['link'] not in data:
                data.append(item['link'])
                swd.value = data
                swd.save()
                output_dict['url'] = item['link']
                break
        else:
            from streams.models import HaltStream
            raise HaltStream("Halting stream.")
    return output_dict


def streaming_sliding_window(input_dict,widget,stream=None):
    from streams.models import StreamWidgetData
    output_dict = {}
    if stream is None:
        output_dict['list']=input_dict['list'][:int(input_dict['size'])]
    else:
        try:
            swd = StreamWidgetData.objects.get(stream=stream,widget=widget)
            data = swd.value
        except:
            swd = StreamWidgetData()
            swd.stream = stream
            swd.widget = widget
            data = []
            swd.value = data
            swd.save()
        size = int(input_dict['size'])
        if len(input_dict['list'])>=size:
            output_dict['list']=input_dict['list'][:size]
            swd.value=output_dict['list']
            swd.save()
        else:
            current_window = input_dict['list'] + swd.value
            swd.value = current_window[:size]
            output_dict['list']=current_window[:size]
            swd.save()
    return output_dict