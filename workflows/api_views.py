from workflows.models import Workflow, Widget
from rest_framework import viewsets
from rest_framework.views import APIView
from rest_framework.response import Response
from workflows.serializers import WorkflowSerializer, WidgetSerializer
#from rest_framework import filters

class WorkflowViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows workflows to be viewed or edited.
    """
    serializer_class = WorkflowSerializer
    model = Workflow

    def pre_save(self, workflow):
        workflow.user = self.request.user

    def get_queryset(self):
        return Workflow.objects.filter(user=self.request.user,widget=None)


class WidgetViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows widgets to be viewed or edited.
    """
    serializer_class = WidgetSerializer
    model = Widget

    def get_queryset(self):
        return Widget.objects.filter(workflow__user=self.request.user)