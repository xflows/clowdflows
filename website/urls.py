from django.conf.urls import patterns, include, url

urlpatterns = patterns('',

    url(r'^$', 'website.views.index', name='website index'),
    url(r'^existing-workflows/', 'website.views.workflows', name='existing workflows'),
    url(r'^your-workflows/', 'website.views.your_workflows', name='your workflows'),
    url(r'^streams/(?P<stream_id>[0-9]+)/$', 'website.views.stream', name='stream'),

    url(r'^start-stream/(?P<workflow_id>[0-9]+)/$', 'website.views.start_stream', name='start stream'),

    url(r'^activate-stream/(?P<stream_id>[0-9]+)/$', 'website.views.activate_stream', name='activate stream'),
    url(r'^deactivate-stream/(?P<stream_id>[0-9]+)/$', 'website.views.deactivate_stream', name='deactivate stream'),

    url(r'^reset-stream/(?P<stream_id>[0-9]+)/$', 'website.views.reset_stream', name='reset stream'),

    url(r'^make-private/(?P<workflow_id>[0-9]+)/$', 'website.views.make_private', name='make private'),
    url(r'^make-public/(?P<workflow_id>[0-9]+)/$', 'website.views.make_public', name='make public'),

    url(r'^export-workflow/(?P<workflow_id>[0-9]+)/$', 'website.views.export_workflow', name='export workflow'),
    url(r'^import-workflow/$', 'website.views.import_workflow', name='import workflow'),
    url(r'^workflow/(?P<workflow_id>[0-9]+)/$', 'website.views.workflow_information', name='workflow information'),

    url(r'^editor/$', 'website.views.editor', name='editor'),

)
