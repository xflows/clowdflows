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

class WorkflowListSerializer(serializers.HyperlinkedModelSerializer):
    is_subprocess = serializers.SerializerMethodField()

    def get_is_subprocess(self, obj):
        if obj.widget == None:
            return False
        else:
            return True

    class Meta:
        model = Workflow
        exclude = ('user',)

class WidgetSerializer(serializers.HyperlinkedModelSerializer):
    inputs = InputSerializer(many=True, read_only=True)
    outputs = OutputSerializer(many=True, read_only=True)
    workflow_link = serializers.HyperlinkedRelatedField(
        read_only=True,
        view_name='workflow-detail'
    )

    class Meta:
        model = Widget
        exclude = ('abstract_widget',)

class WorkflowSerializer(serializers.HyperlinkedModelSerializer):
    widgets = WidgetSerializer(many=True, read_only=True)
    connections = ConnectionSerializer(many=True, read_only=True)
    is_subprocess = serializers.SerializerMethodField()

    def get_is_subprocess(self, obj):
        if obj.widget == None:
            return False
        else:
            return True

    class Meta:
        model = Workflow
        exclude = ('user',)