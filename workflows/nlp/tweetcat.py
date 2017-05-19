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
  return lang in langid_lang


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
  if lang_id(timeline, langid_lang):
    timeline=[]
    try:
      for page in tweepy.Cursor(api.user_timeline,id=screen_name,count=200).pages(16):
        timeline.extend(page)
    except:
      #print(datetime.now().isoformat()+'\tNew user '+screen_name+' did pass the language filter, but could not be retrieved at this point. He will come up again...\n')
      return None
    return sorted(timeline,key=lambda x:x.id)
  else:
    return False


def user_timeline(screen_name,since_id, api):
  try:
    return sorted(api.user_timeline(screen_name,since_id=since_id),key=lambda x:x.id)
  except:
    return


def write_tweets(tweets):
    tweetlist = []
    for tw in tweets:
        tweet = {}
        tweet['id'] = tw.id
        tweet['created_at'] = tw.created_at
        tweet['text'] = unicode(tw.text).encode("utf-8")
        try:
            tweet['user'] = tw.user['screen_name']
            tweet['lang'] = tweet.lang
        except:
            tweet['user'] = ""
            tweet['lang'] = ""
        tweetlist.append(tweet)
    return tweetlist


def lang_mode(seedw, user_index, api, langid_lang):
  no_tweets = 0
  timeout = time() + 60 * 1
  tweets=[]
  last_seed = seedw[0]
  
  # iterating through seed words and searching for users
  for seed in seedw:
    if time() > timeout:
      break
    for hit in search(seed, api):
      if time() > timeout:
        last_seed = seed
        break
      
      if hit.author.screen_name not in user_index:
        fetched_timeline=new_user_timeline(hit.author.screen_name, api, langid_lang)
        if fetched_timeline is False:
          #print(datetime.now().isoformat()+'\tNew user '+hit.author.screen_name+' found by searching did not pass the language filter.\n')
          pass
        elif fetched_timeline is None:
          #print(datetime.now().isoformat()+'\tNew user '+hit.author.screen_name+' found by searching could not be retrieved. He probably has a protected account.\n')
          pass
        else:
          #print(datetime.now().isoformat()+'\tFound new user by searching: '+hit.author.screen_name+'\n')
          user_index[hit.author.screen_name]=fetched_timeline[-1].id
          tweets.extend(fetched_timeline)
          no_tweets+=len(fetched_timeline)
          #tweets=write_tweets(tweets)
          for follower in followers(hit.author):
            if time() > timeout:
              last_seed = seed
              break
            if follower.screen_name not in user_index:
              fetched_timeline=new_user_timeline(follower.screen_name, api, langid_lang)
              if fetched_timeline is False:
                #print(datetime.now().isoformat()+'\tNew user '+follower.screen_name+' found as follower did not pass the language filter.\n')
                pass
              elif fetched_timeline is None:
                #print(datetime.now().isoformat()+'\tNew user '+follower.screen_name+' found as follower could not be retrieved. He probably has a protected account.\n')
                pass
              else:
                #print(datetime.now().isoformat()+'\tFound new user through followers: '+follower.screen_name+'\n')
                user_index[follower.screen_name]=fetched_timeline[-1].id
                tweets.extend(fetched_timeline)
                if time() > timeout:
                    break
                no_tweets+=len(fetched_timeline)
                #tweets=write_tweets(tweets)
          for friend in friends(hit.author):
            if time() > timeout:
              last_seed = seed
              break
            if friend.screen_name not in user_index:
              fetched_timeline=new_user_timeline(friend.screen_name, api, langid_lang)
              if fetched_timeline is False:
                #print(datetime.now().isoformat()+'\tNew user '+friend.screen_name+' found as friend did not pass the language filter.\n')
                pass
              elif fetched_timeline is None:
                #print(datetime.now().isoformat()+'\tNew user '+friend.screen_name+' found as friend could not be retrieved. He probably has a protected account.\n')
                pass
              else:
                #print(datetime.now().isoformat()+'\tFound new user through friends: '+friend.screen_name+'\n')
                user_index[friend.screen_name]=fetched_timeline[-1].id
                tweets.extend(fetched_timeline)
                no_tweets+=len(fetched_timeline)
                #tweets=write_tweets(tweets)
      
  # iterating through all known users
  for screen_name,since_id in user_index.items():
    if time() > timeout:
      last_seed = seed
      break
    fetched_timeline=user_timeline(screen_name,since_id, api)
    if fetched_timeline is None:
      pass
      #print(datetime.now().isoformat()+'\tKnown user\'s '+screen_name+' timeline not fetched. Will continue on trying.\n')
      continue
    else:
      if len(fetched_timeline)>0:
        #print(datetime.now().isoformat()+'\tKnown user\'s '+screen_name+' timeline fetched with '+str(len(fetched_timeline))+' new tweets.\n')
        user_index[screen_name]=fetched_timeline[-1].id
        tweets.extend(fetched_timeline)
        no_tweets+=len(fetched_timeline)
        tweets=write_tweets(tweets)
      else:
        pass
        #print(datetime.now().isoformat()+'\tKnown user\'s '+screen_name+' timeline fetched with no new tweets.\n')

  tweets=write_tweets(tweets)
  return (tweets, user_index, last_seed)


class StdOutListener(StreamListener):

  def __init__(self):
    super(StdOutListener, self).__init__()
    self.tweets=[]
    self.tweetList = []

  def on_status(self,status):
    if status.coordinates!=None:
      #print status.text
      self.tweets.append(status)
      #print(len(self.tweets))
      self.tweetList=write_tweets(self.tweets)

  def on_error(self,status):
    #print(datetime.now().isoformat()+'\tERROR 420, sleeping '+str(5)+'\n')
    sleep(5)
   

  