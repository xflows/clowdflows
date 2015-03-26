import json
from django.shortcuts import render


def base_js_snippet(request, input_dict, output_dict, widget):
    try:
        inputs = json.dumps(input_dict['in'])
    except:
        raise Exception("Problem serializing the inputs. Only JSON-serializable objects can be used.")
    return render(request, 'interactions/base_js_snippet.html',
                  {'widget': widget, 'snippet': input_dict['snippet'], 'inputs': inputs})
