from django.conf.urls import patterns, include, url
from rest_framework.urlpatterns import format_suffix_patterns

urlpatterns = patterns('',
    url(r'^api-auth/', include('rest_framework.urls', namespace='rest_framework')),
    url(r'^api-token-auth/?', 'rest_framework.authtoken.views.obtain_auth_token'),
)

urlpatterns = format_suffix_patterns(urlpatterns)