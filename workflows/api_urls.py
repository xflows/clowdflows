from django.conf.urls import patterns, include, url
from rest_framework import routers
from workflows import api_views

router = routers.DefaultRouter()
router.register(r'workflows', api_views.WorkflowViewSet, base_name='workflow')
router.register(r'widgets', api_views.WidgetViewSet, base_name='widget')
router.register(r'connections', api_views.ConnectionViewSet, base_name='connection')
router.register(r'inputs', api_views.InputViewSet, base_name='input')
router.register(r'outputs', api_views.OutputViewSet, base_name='output')


urlpatterns = patterns('',
    url(r'^', include(router.urls)),
    url(r'^api-auth/', include('rest_framework.urls', namespace='rest_framework'))
)
