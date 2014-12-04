from rest_framework import viewsets, mixins
from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework import filters
from workflows.models import *
from workflows.serializers import *


class WorkflowViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows workflows to be viewed or edited.
    """
    model = Workflow
    filter_fields = ('public',)

    def get_serializer_class(self):
        if self.action == 'list':
            return WorkflowListSerializer
        return WorkflowSerializer

    def perform_create(self, serializer):
        serializer.save(user=self.request.user)

    def get_queryset(self):
        return Workflow.objects.filter(user=self.request.user).prefetch_related('widgets','widgets__inputs','widgets__outputs')


class WidgetViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows widgets to be viewed or edited.
    """
    model = Widget
    filter_fields = ('workflow',)

    def get_serializer_class(self):
        if self.action == 'list':
            return WidgetListSerializer
        return WidgetSerializer

    def get_queryset(self):
        return Widget.objects.filter(workflow__user=self.request.user).prefetch_related('inputs','outputs')


class ConnectionViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows connections to be viewed or edited.
    """
    serializer_class = ConnectionSerializer
    model = Connection

    def get_queryset(self):
        return Connection.objects.filter(workflow__user=self.request.user)


class InputViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows widget inputs to be viewed or edited.
    """
    serializer_class = InputSerializer
    model = Input

    def get_queryset(self):
        return Input.objects.filter(widget__workflow__user=self.request.user)


class OutputViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows widget outputs to be viewed or edited.
    """
    serializer_class = OutputSerializer
    model = Output

    def get_queryset(self):
        return Output.objects.filter(widget__workflow__user=self.request.user)
