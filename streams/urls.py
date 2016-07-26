from django.conf.urls import patterns, include, url

urlpatterns = patterns('',
    url(r'^data/(?P<stream_id>[0-9]+)/(?P<widget_id>[0-9]+)/$', 'streams.views.stream_widget_visualization', name='stream widget visualization'),
)
