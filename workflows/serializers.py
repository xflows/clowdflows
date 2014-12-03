from workflows.models import Workflow, Widget, Connection, Input, Output, Option
from rest_framework import serializers
#from rest_framework.reverse import reverse


class WidgetSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Widget


class WorkflowListSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Workflow
        fields = ('id', 'name', 'public', 'description', 'url')


class WorkflowSerializer(serializers.HyperlinkedModelSerializer):
    widgets = WidgetSerializer(many=True,read_only=True)
    class Meta:
        model = Workflow
        fields = ('id', 'name', 'public', 'description', 'widgets', 'url')


class ConnectionSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Connection


class InputSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Input
        exclude = ('value',)

class OutputSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Output
        exclude = ('value',)
