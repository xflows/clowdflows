from django.conf.urls import patterns, include, url
from rest_framework import routers
from workflows import api_views

router = routers.DefaultRouter()
router.register(r'workflows', api_views.WorkflowViewSet, base_name='workflows')
router.register(r'widgets', api_views.WidgetViewSet, base_name='widgets')
router.register(r'connections', api_views.ConnectionViewSet, base_name='connections')
router.register(r'inputs', api_views.InputViewSet, base_name='inputs')
router.register(r'outputs', api_views.OutputViewSet, base_name='outputs')


urlpatterns = patterns('',
    url(r'^', include(router.urls)),
    url(r'^api-auth/', include('rest_framework.urls', namespace='rest_framework'))
)
