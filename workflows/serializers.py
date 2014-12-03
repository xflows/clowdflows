from rest_framework import serializers
from workflows.models import *
#from rest_framework.reverse import reverse


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


class WidgetListSerializer(serializers.HyperlinkedModelSerializer):
   class Meta:
       model = Widget
       exclude = ('abstract_widget',)


class WidgetSerializer(serializers.HyperlinkedModelSerializer):
    inputs = InputSerializer(many=True, read_only=True)
    outputs = OutputSerializer(many=True, read_only=True)
    class Meta:
        model = Widget
        exclude = ('abstract_widget',)


class WorkflowListSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Workflow
        fields = ('id', 'name', 'public', 'description', 'url')


class WorkflowSerializer(serializers.HyperlinkedModelSerializer):
    widgets = WidgetSerializer(many=True, read_only=True)
    class Meta:
        model = Workflow
        fields = ('id', 'name', 'public', 'description', 'widgets', 'url')
