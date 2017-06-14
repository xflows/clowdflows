import sys
import tweepy
from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from time import sleep,time
from datetime import datetime
from xml.sax.saxutils import escape
from shutil import move,copy
import cPickle as pickle
import re
import os.path
import langid
import json
import gzip

def clean(timeline):
  import re
  stuff=re.compile(r'\bhttp\S+|\b@\w+|\b#\w+',re.UNICODE)
  return ' '.join([' '.join(re.findall(r'\w+',stuff.sub(' ',e.text.lower()),re.UNICODE)) for e in timeline])


def lang_id(timeline, langid_lang):
  timeline=clean(timeline)
  lang=langid.classify(timeline)[0]
  #print(datetime.now().isoformat()+'\t'+repr(timeline[:200])+' identified as '+lang+'\n')
  if lang in langid_lang:
    return lang
  return False


def search(term, api):
  try:
    result=api.search(term)
  except:
    return []
  return result


def followers(user):
  try:
    result=user.followers()
  except:
    return []
  return result


def friends(user):
  try:
    result=user.friends()
  except:
    return []
  return result


def new_user_timeline(screen_name, api, langid_lang):
  try:
    timeline=[e for e in tweepy.Cursor(api.user_timeline,id=screen_name).items(200)]
  except:
    return None
  if len(timeline)<100:
    #print(datetime.now().isoformat()+'\tNew user '+screen_name+' did not pass the tweet number threshold ('+str(len(timeline))+') for accurate language identification.\n')
    return False
  language = lang_id(timeline, langid_lang)
  if language:
    timeline=[]
    try:
      for page in tweepy.Cursor(api.user_timeline,id=screen_name,count=200).pages(16):
        timeline.extend(page)
    except:
      #print(datetime.now().isoformat()+'\tNew user '+screen_name+' did pass the language filter, but could not be retrieved at this point. He will come up again...\n')
      return None
    return (sorted(timeline,key=lambda x:x.id), language)
  else:
    return False


def user_timeline(screen_name,since_id, api):
  try:
    return sorted(api.user_timeline(screen_name,since_id=since_id),key=lambda x:x.id)
  except:
    return

def write_tweets(tweets, user_lang=None):
    tweetlist = []
    for tw in tweets:
        tweet = {}
        tweet['id'] = tw.id
        tweet['created_at'] = tw.created_at
        tweet['text'] = unicode(tw.text).encode("utf-8")
        tweet['user'] = tw.user.screen_name
        if user_lang:
          tweet['lang'] = user_lang[tweet['user']]
        else:
          try:
            tweet['lang'] = tw.lang
          except:
            tweet['lang'] = ""
        tweetlist.append(tweet)
    return tweetlist


def set_search_state(seeds=(None, 0), hits=(None, 0), followers=(None, 0), friends=(None, 0)):
  seeds, index = seeds
  if seeds != None:
    seeds = None if len(seeds) <= index else seeds[index:]

  hits, index = hits
  if hits != None:
    if len(hits) == 0:
      hits = []
    else:
      hits = None if len(hits) <= index else hits[index:]

  followers, index = followers
  if followers != None:
    if len(followers) == 0:
      followers = []
    else:
      followers = None if len(followers) <= index else followers[index:]
  
  friends, index = friends
  if friends != None:
    if len(friends) == 0:
      friends = []
    else:
      friends = None if len(friends) <= index else friends[index:]
  return {'seeds': seeds, 'hits': hits, 'followers': followers, 'friends': friends}


def lang_mode(state, user_index, api, langid_lang, user_lang, seeds_searched=False):
  tweets=[]
  if not seeds_searched:
    seeds = state['seeds']
    hits = state['hits']
    foll = state['followers']
    fri = state['friends']
    if hits == None:
      hits = search(seeds[0], api)
    for i, hit in enumerate(hits):
      if hit.author.screen_name not in user_index:
        if foll == None and fri == None:
          fetched_timeline = new_user_timeline(hit.author.screen_name, api, langid_lang)
        if foll == None and fri == None and (fetched_timeline is False or fetched_timeline is None):
          if len(hits) == i +1:
            data = set_search_state(seeds=(seeds, 1), hits=(hits, i + 1))
          else:
            data = set_search_state(seeds=(seeds, 0), hits=(hits, i + 1))
          return (tweets, user_index, user_lang, data)
        else:
          if foll == None and fri == None:
            fetched_timeline, language = fetched_timeline
            user_lang[hit.author.screen_name] = language
            user_index[hit.author.screen_name]=fetched_timeline[-1].id
            tweets.extend(write_tweets(fetched_timeline, user_lang))
            foll = followers(hit.author)
            data = set_search_state((seeds, 0), (hits, i), (foll, 0), (fri, 0))
            return (tweets, user_index, user_lang, data)
          if fri == None:
            for j, follower in enumerate(foll):
              if follower.screen_name not in user_index:
                fetched_timeline = new_user_timeline(follower.screen_name, api, langid_lang)
                if fetched_timeline is False or fetched_timeline is None:
                  data = set_search_state((seeds, 0), (hits, i), (foll, j + 1), (fri, 0))
                  return (tweets, user_index, user_lang, data)
                else:
                  fetched_timeline, language = fetched_timeline
                  user_lang[follower.screen_name] = language
                  user_index[follower.screen_name]=fetched_timeline[-1].id
                  tweets.extend(write_tweets(fetched_timeline, user_lang))
                  if j == len(foll) - 1:
                    fri = friends(hit.author)
                  data = set_search_state((seeds, 0), (hits, i), (foll, j + 1), (fri, 0))
                  return (tweets, user_index, user_lang, data)
            if fri == None:
              fri = friends(hit.author)
          for j, friend in enumerate(fri):
            if friend.screen_name not in user_index:
              fetched_timeline = new_user_timeline(friend.screen_name, api, langid_lang)
              if fetched_timeline is False or fetched_timeline is None:
                data = set_search_state((seeds, 0), (hits, i), (foll, 0), (fri, j + 1))
                return (tweets, user_index, user_lang, data)
              else:
                fetched_timeline, language = fetched_timeline
                user_lang[friend.screen_name] = language
                user_index[friend.screen_name]=fetched_timeline[-1].id
                tweets.extend(write_tweets(fetched_timeline, user_lang))
                data = set_search_state((seeds, 0), (hits, i), (foll, 0), (fri, j + 1))
                return (tweets, user_index, user_lang, data)
          foll = None
          fri = None     
    data = set_search_state(seeds=(seeds, 1))
    return (tweets, user_index, user_lang, data)
  else:
    # iterating through all known users
    for screen_name,since_id in user_index.items():
      fetched_timeline=user_timeline(screen_name,since_id, api)
      if fetched_timeline is None:
        continue
      else:
        if len(fetched_timeline)>0:
          user_index[screen_name]=fetched_timeline[-1].id
          tweets.extend(write_tweets(fetched_timeline, user_lang))
    data = {'seeds': None, 'hits': None, 'followers': None, 'friends': None}
    return (tweets, user_index, user_lang, data)


class StdOutListener(StreamListener):

  def __init__(self):
    self.start_time = time()
    self.limit = 5
    super(StdOutListener, self).__init__()
    self.tweets=[]
    self.tweetList = []

  def on_status(self,status):
    if status.coordinates!=None:
      self.tweets.append(status)
      self.tweetList=write_tweets(self.tweets)
    if (time() - self.start_time) < self.limit:
      return True
    else:
      return False


  def on_error(self,status):
    sleep(5)
   

  