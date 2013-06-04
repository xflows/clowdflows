'''
Streaming widgets librarby

@author: Janez Kranjc <janez.kranjc@ijs.si>
'''

def streaming_twitter(input_dict,widget,stream=None):
    output_dict = {}
    return output_dict

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
