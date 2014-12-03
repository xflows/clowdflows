from workflows.models import Workflow
from rest_framework import serializers
#from rest_framework.reverse import reverse


class WorkflowSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Workflow
        fields = ('id', 'name', 'public', 'description', 'url')