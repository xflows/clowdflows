from django.conf.urls import patterns, include, url

urlpatterns = patterns('',

    url(r'^signuplogin/$', 'signuplogin.views.signuplogin', name='signuplogin'),
    
    
)