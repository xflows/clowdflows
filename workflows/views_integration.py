import json

from django.conf import settings
from django.contrib.auth.models import User, Group
from django.shortcuts import redirect
from rest_framework import status
from rest_framework.response import Response
from rest_framework.views import APIView

from .models import Workflow


def is_pd_man_admin(user):
    return user.groups.filter(name='PD_Manager_Admin').exists()

def is_pd_man_researcher(user):
    return user.groups.filter(name='PD_Manager_Researcher').exists()


class RegisterPdManUser(object):
    """
    Helper class for registering a new PD_Manager user and creating a workflow
    """

    def __init__(self, data):
        self.data = data
        pass

    def add_user_to_pd_man_researcher_group(self):
        g = Group.objects.get(name='PD_Manager_Researcher')
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

            self.add_user_to_pd_man_researcher_group()
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


class RegisterPdManUserRESTView(APIView):
    """
    Integration with PD_Manager Researcher's App: Registers a new PD_Manager user and creates a workflow
    """

    def post(self, request, *args, **kw):
        if request.user.is_authenticated() and is_pd_man_admin(request.user):
            # see custom DRF permission

            # check size of group [PD_Manager_Researcher]
            n = len(Group.objects.get(name='PD_Manager_Researcher').user_set.all())
            print("Num [PD_Manager_Researcher] users: %d" % n)
            if len(Group.objects.get(name='PD_Manager_Researcher').user_set.all()) > 200:
                response = Response({'detail': 'Too many researchers registered'}, status=status.HTTP_303_SEE_OTHER)
                return response

            data = {'username': request.POST.get('username', None),
                    'password': request.POST.get('password', None),
                    'first_name': request.POST.get('first_name', None),
                    'last_name': request.POST.get('last_name', None),
                    'email': request.POST.get('email', None)}

            registerPdManUserClass = RegisterPdManUser(data)
            registerPdManUserClass.do_register()
            workflow_id = registerPdManUserClass.create_workflow()

            response = Response({'workflow_id': workflow_id}, status=status.HTTP_200_OK)
        else:
            response = Response({'detail': 'Not authorized'}, status=status.HTTP_401_UNAUTHORIZED)

        return response


class DatasetCreatedPdManRESTView(APIView):
    """
    Integration with PD_Manager Researcher's App: Update the id of the dataset
    """
    def post(self, request, *args, **kw):
        if request.user.is_authenticated() and is_pd_man_admin(request.user):
            # See custom DRF permission

            # Find the workflow, find the widget [Import PD_Manager data] and update its setting

            data = {'username': request.POST.get('username', None),
                    'workflow_id': request.POST.get('workflow_id', None)}

            registerPdManUserClass = RegisterPdManUser(data)
            registerPdManUserClass.do_register()
            workflow_id = registerPdManUserClass.create_empty_workflow()

            response = Response({'workflow_id': workflow_id}, status=status.HTTP_200_OK)
        else:
            response = Response({}, status=status.HTTP_401_UNAUTHORIZED)

        return response


def login_and_edit_workflow(request, workflow_id, username, password):

    # data = {'username': request.POST.get('username', None),
    #         'password': request.POST.get('password', None),
    #         'workflow_id': request.POST.get('workflow_id', None)}

    data = {'username': username,
            'password': password,
            'workflow_id': workflow_id}

    # workflow_id = request.POST.get("workflow_id")

    # expect and check username and password od [PD_Man_res] user..
    from views_integration import is_pd_man_researcher

    list_users = User.objects.filter(username=data['username'])
    if not(len(list_users) == 1):
        raise Exception("User not found")
    user = list_users[0]

    if user.check_password(data['password']):
        if is_pd_man_researcher(user):

            list_workflows = Workflow.objects.filter(id=workflow_id, user__username=data['username'])

            if not(len(list_workflows) == 1):
                raise Exception("Wrong workflow ID")

            workflow = list_workflows[0]
            if workflow:
                user = workflow.user

                user.backend = settings.AUTHENTICATION_BACKENDS[0]
                from django.contrib.auth import login
                login(request, user)

                user.userprofile.active_workflow = workflow
                user.userprofile.save()
                return redirect('editor')
