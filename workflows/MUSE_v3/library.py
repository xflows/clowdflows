import re
import subprocess
import logging
try:
    from ladon.clients.jsonwsp import JSONWSPClient
except ImportError:
    logging.warning('Ladon package not available, most of MUSE stuff will not work.')



def MUSE_preprocessing(input_dict):
    url = input_dict['url']
    text = input_dict['text']

    import socket
    socket.setdefaulttimeout(None)

    cli = JSONWSPClient(url)
    result = cli.preprocessing(text=text)
    tokens = result.response_dict['result']
    return {'tokens': tokens}
#end

