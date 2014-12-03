from django.conf.urls import patterns, include, url
from rest_framework import routers
from workflows import api_views

router = routers.DefaultRouter()
router.register(r'workflows', api_views.WorkflowViewSet)
router.register(r'widgets', api_views.WidgetViewSet)
router.register(r'connections', api_views.ConnectionViewSet)
router.register(r'inputs', api_views.InputViewSet)
router.register(r'outputs', api_views.OutputViewSet)


urlpatterns = patterns('',
    url(r'^', include(router.urls)),
    url(r'^api-auth/', include('rest_framework.urls', namespace='rest_framework'))
)
