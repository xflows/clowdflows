# Tweepy
# Copyright 2009-2010 Joshua Roesslein
# See LICENSE for details.

"""
Tweepy Twitter API library
"""
__version__ = '2.0'
__author__ = 'Joshua Roesslein'
__license__ = 'MIT'

from tweepy2.models import Status, User, DirectMessage, Friendship, SavedSearch, SearchResult, ModelFactory, Category
from tweepy2.error import TweepError
from tweepy2.api import API
from tweepy2.cache import Cache, MemoryCache, FileCache
from tweepy2.auth import BasicAuthHandler, OAuthHandler
from tweepy2.streaming import Stream, StreamListener
from tweepy2.cursor import Cursor

# Global, unauthenticated instance of API
api = API()

def debug(enable=True, level=1):

    import httplib
    httplib.HTTPConnection.debuglevel = level

