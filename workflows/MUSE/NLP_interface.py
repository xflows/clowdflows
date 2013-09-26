#import requests

import urllib
import urllib2
import HTMLParser
from xml.sax import saxutils as su

TERENCE_SERVICE_URL = 'http://ariadne.cs.kuleuven.be/TERENCEStoryService/rest/auto'


def invoke_terence_service(text):
    submitVars = {'text': text}
    submitVarsUrlencoded = urllib.urlencode(submitVars)
    req = urllib2.Request(TERENCE_SERVICE_URL, submitVarsUrlencoded)
    response = urllib2.urlopen(req)
    xml = response.read()

    #print repr(xml)
    #xml = su.unescape(xml)
    #print '----------'
    #print repr(xml)
    #html_parser = HTMLParser.HTMLParser()
    #xml = html_parser.unescape(xml)
    return xml
