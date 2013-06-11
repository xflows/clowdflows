# -*- coding: utf-8 -*-
'''
Streaming widgets librarby

@author: Janez Kranjc <janez.kranjc@ijs.si>
'''

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

    client = SoapClient(location = "http://batman.ijs.si:8008/",action = 'http://batman.ijs.si:8008/',namespace = "http://example.com/tweetsentiment.wsdl",soap_ns='soap',trace = False,ns = False)

    list_of_tweets = input_dict['ltw']

    new_list_of_tweets = []

    for tweet in list_of_tweets:
        new_list_of_tweets.append({'id':tweet['id'],'text':tweet['text'],'language':tweet['lang']})

    pickled_list_of_tweets = pickle.dumps(new_list_of_tweets)

    response = client.TweetSentimentService(tweets=pickled_list_of_tweets)

    new_ltw = pickle.loads(unicode(response.TweetSentimentResult))

    i=0
    for new_tweet in new_ltw:
        list_of_tweets[i]['sentiment']=new_tweet['sentiment']
        list_of_tweets[i]['lang']=new_tweet['language']
        list_of_tweets[i]['reliability']=new_tweet['reliability']
        i = i + 1

    output_dict = {}

    output_dict['ltw'] = list_of_tweets

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
