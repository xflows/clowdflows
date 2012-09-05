from django.conf.urls.defaults import patterns, include, url

urlpatterns = patterns('',

    url(r'^$', 'website.views.index', name='website index'),
    url(r'^existing-workflows/', 'website.views.workflows', name='existing workflows'),
    
    url(r'^workflow/(?P<workflow_id>[0-9]+)/$', 'website.views.workflow_information', name='workflow information'),    
    
)