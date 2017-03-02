import json

from django.conf import settings
from django.contrib.auth.models import User, Group
from django.shortcuts import redirect
from rest_framework import status
from rest_framework.response import Response
from rest_framework.views import APIView
from rest_framework.permissions import BasePermission

from rest_framework.decorators import permission_classes

from .models import Workflow, Widget, AbstractWidget, Input


def is_pd_man_admin(user):
    return user.groups.filter(name='PD_Manager_Admin').exists()

def is_pd_man_researcher(user):
    return user.groups.filter(name='PD_Manager_Researcher').exists()

# ====================
# Helper classes
# ====================
class RegisterUserAndCreateWorkflow(object):
    """
    Helper class for registering a new user and creating a workflow
    """

    def __init__(self, data):
        self.data = data
        pass

    def add_user_to_group(self, group_name):
        g = Group.objects.get(name=group_name)
        g.user_set.add(self.user)
        g.save()

    def do_register(self):
        # STEP 1 : Create a user

        # Check if this user exists
        list_exising_users = User.objects.filter(username=self.data['username'])

        if len(list_exising_users) == 0:
            self.user = User(
                first_name=self.data['first_name'],
                last_name=self.data['last_name'],
                email=self.data['email'],
                username=self.data['username'],
                # optional:
                # dataset_id=self.data['dataset_id']
            )
            p = str(self.data['password'])
            self.user.set_password(p)
            self.user.save()

            if self.data['group'] is not None:
                self.add_user_to_group(self.data['group'])
            self.user.save()
        else:
            raise Exception("A user with username %s already exists" % self.data['username'])


    def create_workflow(self):
        # STEP 2 : Create a workflow

        json_data = json.loads(open("workflows/pdmanager/predefined_workflow.json").read())

        w = Workflow()
        w.user = self.user
        w.public = False
        w.import_from_json(json_data,{},{})
        w.name = "PD_Manager Workflow - Data analysis - %s" % (self.user.username)

        w.save()

        return w.id

# ====================
# Permissions
# ====================

class RegisterUserPermission(BasePermission):

    def has_permission(self, request, view):

        return 'auth.add_user' in request.user.get_all_permissions()
        # return request.user.is_authenticated() and 'auth.add_user' in request.user.get_all_permissions()


# ====================
# API Views
# ====================

class RegisterUserAndCreateWorkflowAPIView(APIView):
    """
    An API view which registers a new user, puts it in a given group, and creates its first workflow
    """

    permission_classes = (RegisterUserPermission, )

    def post(self, request, *args, **kw):

        data = {'username': request.POST.get('username', None),
                'password': request.POST.get('password', None),
                'first_name': request.POST.get('first_name', None),
                'last_name': request.POST.get('last_name', None),
                'email': request.POST.get('email', None),
                'group': request.POST.get('group', None)}  #'PD_Manager_Researcher'

        # check size of group
        n = len(Group.objects.get(name=data['group']).user_set.all())
        print("Num [%s] users: %d" % (data['group'], n))
        if n > 200:
            response = Response({'detail': 'Too many users in group'}, status=status.HTTP_303_SEE_OTHER)
            return response

        registerHelperClass = RegisterUserAndCreateWorkflow(data)
        registerHelperClass.do_register()
        workflow_id = registerHelperClass.create_workflow()

        response = Response({'workflow_id': workflow_id}, status=status.HTTP_200_OK)

        return response


class ModifyWidgetRESTView(APIView):
    """
    An API view which modifies one widget of a workflow
    """

    permission_classes = (RegisterUserPermission, )

    def post(self, request, *args, **kw):

        data = {'widget_name': request.POST.get('widget_name', None),
                'workflow_id': request.POST.get('workflow_id', None),
                'dataset_id': request.POST.get('dataset_id', None)}

        aw = AbstractWidget.objects.filter(name='Import PD_Manager data', package='pdmanager')[0]

        # Find the workflow, find the widget [Import PD_Manager data] and update its setting
        wf = Workflow.objects.get(id=data['workflow_id'])
        if wf:
            import_data_widget = Widget.objects.filter(workflow__id=wf.id, abstract_widget=aw)[0]

            # TODO Filter input by name ?
            inp_param_dataset_id = Input.objects.filter(widget=import_data_widget)[0]
            inp_param_dataset_id.value = data['dataset_id']
            inp_param_dataset_id.save()

            response = Response({'detail': 'OK'}, status=status.HTTP_200_OK)
        else:
            response = Response({'detail': 'Wrong workflow ID'}, status=status.HTTP_401_UNAUTHORIZED)


        return response


# ====================
# Views
# ====================

def login_and_edit_workflow(request, workflow_id, username, password):

    # data = {'username': request.POST.get('username', None),
    #         'password': request.POST.get('password', None),
    #         'workflow_id': request.POST.get('workflow_id', None)}

    data = {'username': username,
            'password': password,
            'workflow_id': workflow_id}

    # expect and check username and password of [PD_Man_res] user..
    from views_integration import is_pd_man_researcher

    list_users = User.objects.filter(username=data['username'])
    if len(list_users) >= 1:

        user = list_users[0]
        if user.check_password(data['password']) and is_pd_man_researcher(user):
            list_workflows = Workflow.objects.filter(id=workflow_id, user__username=data['username'])
            if len(list_workflows) >= 1:
                workflow = list_workflows[0]

                user = workflow.user

                user.backend = settings.AUTHENTICATION_BACKENDS[0]
                from django.contrib.auth import login
                login(request, user)

                user.userprofile.active_workflow = workflow
                user.userprofile.save()
                return redirect('editor')
            else:
                response = Response({'detail': 'Wrong workflow ID'}, status=status.HTTP_303_SEE_OTHER)

        else:
            response = Response({'detail': 'Authentication/authorization problem'}, status=status.HTTP_303_SEE_OTHER)

    else:
        response = Response({'detail': 'User not found'}, status=status.HTTP_303_SEE_OTHER)

    return response
