from workflows.models import Workflow, Widget
from rest_framework import serializers
#from rest_framework.reverse import reverse


class WorkflowSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Workflow
        fields = ('id', 'name', 'public', 'description', 'url')


class WidgetSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Widget
        fields = ('id', 'workflow', 'x', 'y', 'name', 'finished', 'error',
                  'running', 'interaction_waiting', 'type', 'progress')
