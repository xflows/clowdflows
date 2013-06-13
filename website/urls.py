from django.conf.urls.defaults import patterns, include, url

urlpatterns = patterns('',

    url(r'^$', 'website.views.index', name='website index'),
    url(r'^existing-workflows/', 'website.views.workflows', name='existing workflows'),
    url(r'^your-workflows/', 'website.views.your_workflows', name='your workflows'),
    url(r'^streams/(?P<stream_id>[0-9]+)/$', 'website.views.stream', name='stream'),

    url(r'^make-private/(?P<workflow_id>[0-9]+)/$', 'website.views.make_private', name='make private'),
    url(r'^make-public/(?P<workflow_id>[0-9]+)/$', 'website.views.make_public', name='make public'),

    url(r'^workflow/(?P<workflow_id>[0-9]+)/$', 'website.views.workflow_information', name='workflow information'),

)
