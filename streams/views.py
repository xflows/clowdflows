# helpers, context stuff
from django.http import Http404
from django.shortcuts import get_object_or_404

import workflows.interaction_views
import workflows.visualization_views

from workflows.models import *

from workflows.utils import *

from streams.models import Stream

import workflows.views

# rest
def stream_widget_visualization(request,stream_id,widget_id):
    stream = get_object_or_404(Stream,pk=stream_id)
    widget = get_object_or_404(Widget,pk=widget_id)
    if widget.abstract_widget.streaming_visualization_view == '':
        return Http404
    else:
        view_to_call = getattr(workflows.views,widget.abstract_widget.streaming_visualization_view)
        return view_to_call(request,widget,stream)

