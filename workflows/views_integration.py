import json

from django.conf import settings
from django.contrib.auth.models import User
from django.shortcuts import redirect
from rest_framework.views import APIView

from .models import Workflow


# ====================
# Helper classes
# ====================
class CreateWorkflow(object):
    """
    Helper class for creating a workflow for an existing CF user
    """

    def __init__(self, username):
        self.username = username
        u_l = User.objects.filter(username=username)
        self.user = u_l[0]
        pass

    def create_workflow(self):
        # Create a workflow

        json_data = json.loads(open("workflows/pdmanager/predefined_workflow.json").read())

        w = Workflow()
        w.user = self.user
        w.public = False
        w.import_from_json(json_data,{},{})
        w.name = "PD_Manager Workflow - Data analysis - %s" % (self.user.username)

        w.save()

        return w.id


# ====================
# API Views
# ====================

class CreateWorkflowAPIView(APIView):
    """
    An API view which creates a predifened workflow
    """


    def get(self, request, *args, **kw):

        data = {'secret': request.GET.get('secret', None)}
        if data['secret'] == settings.PD_MANAGER_SECRET:
            username = settings.PD_MANAGER_CF_USERNAME

            registerHelperClass = CreateWorkflow(username)
            workflow_id = registerHelperClass.create_workflow()

            new_workflow = Workflow.objects.get(id=workflow_id)

            # here

            return redirect(new_workflow.get_absolute_url())

