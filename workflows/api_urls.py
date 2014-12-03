from django.conf.urls import patterns, include, url
from rest_framework import routers
from workflows import api_views

router = routers.DefaultRouter()
router.register(r'workflows', api_views.WorkflowViewSet)

urlpatterns = patterns('',
    url(r'^', include(router.urls)),
    url(r'^api-auth/', include('rest_framework.urls', namespace='rest_framework'))
)