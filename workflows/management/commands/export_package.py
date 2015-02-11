from unicodedata import category
from django.core.management.base import BaseCommand, CommandError
from workflows.models import Category, AbstractWidget, AbstractInput, AbstractOutput, AbstractOption
from django.core import serializers
from optparse import make_option
import uuid
import os
import sys
from django.conf import settings
import json

def add_category(category,categories):
    categories.add(category.pk)
    if category.parent:
        add_category(category.parent,categories)

def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)

def choice(choices,question="Your choice: "):
    choice = None
    while 1:
        if not choice:
            input_msg = ""
            for i in range(0,len(choices)):
                input_msg += "["+str(i)+"] "+str(choices[i])+"\n"
            choice_number = raw_input(input_msg + question)
        try:
            choice = choices[int(choice_number)]
            return choice
        except:
            sys.stderr.write("Error: Wrong choice.\n")

def serialize_widget(aw):
    data = json.loads(serializers.serialize("json",[aw,]))[0]
    if data.has_key('pk'):
        data.pop('pk')
    if data['fields'].has_key('user'):
        data['fields'].pop('user')
    if not data['fields']['category'] is None:
        data['fields']['category'] = aw.category.uid
    input_data = json.loads(serializers.serialize("json",aw.inputs.all().order_by('uid')))
    for i in input_data:
        if i.has_key('pk'):
            i.pop('pk')
        i['fields']['widget']=aw.uid
    output_data = json.loads(serializers.serialize("json",aw.outputs.all().order_by('uid')))
    for i in output_data:
        if i.has_key('pk'):
            i.pop('pk')
        i['fields']['widget']=aw.uid
    options_data = json.loads(serializers.serialize("json",AbstractOption.objects.filter(abstract_input__widget=aw).order_by('uid')))
    for o in options_data:
        if o.has_key('pk'):
            o.pop('pk')
        o['fields']['abstract_input']=AbstractInput.objects.get(id=o['fields']['abstract_input']).uid
    return [data,]+input_data+output_data+options_data

def serialize_category(c):
    data = json.loads(serializers.serialize("json",[c,]))[0]
    if data.has_key('pk'):
        data.pop('pk')
    if not data['fields']['parent'] is None:
        c2 = Category.objects.get(id=data['fields']['parent'])
        data['fields']['parent'] = c2.uid
    if data['fields'].has_key('workflow'):
        data['fields'].pop('workflow')
    if data['fields'].has_key('user'):
        data['fields'].pop('user')
    return data

def export_package(package_name,writer):
    if 'workflows.'+package_name not in settings.INSTALLED_APPS:
        raise CommandError("Package not found in INSTALLED_APPS.")

    #here we check the integrity of the package
    aws = AbstractWidget.objects.filter(package=package_name)
    for aw in aws:
        if aw.uid:
            for bw in aws:
                if bw.uid == aw.uid and bw.id != aw.id:
                    writer.write("Found two widgets with the same UID. Please select a widget to assign new UID to.\n")
                    selected_widget = choice([aw,bw],"Select a widget: ")
                    selected_widget.set_uid(commit=True)
                    

    #first we check if package_data directory exists and make it if it doesn't
    package_directory = os.path.join(os.path.dirname(os.path.realpath(__file__)),'../../'+package_name+"/package_data/")
    ensure_dir(package_directory)
    widgets_directory = os.path.join(package_directory,"widgets")
    ensure_dir(widgets_directory)
    categories_directory = os.path.join(package_directory,"categories")
    ensure_dir(categories_directory)
    writer.write(" > Ensuring package directory for "+package_name+".\n")

    categories = set()

    writer.write("   > Exporting widgets\n")

    global_change = False

    for aw in aws:
        aw.update_uid()
        add_category(aw.category,categories)
        serialized_widget = serialize_widget(aw)
        
        created = True
        change = True
        try:
            widget_file = open(os.path.join(widgets_directory,aw.uid+'.json'),'r')
            created = False
            w_data = json.loads(widget_file.read())
            widget_file.close()
            if w_data == serialized_widget:
                change = False
        except:
            created = True
            change = True
        
        if change:
            global_change = True
            if created:
                writer.write("     + Exporting widget "+str(aw)+"\n")
            else:
                writer.write("     + Updating widget "+str(aw)+"\n")
            widget_data = json.dumps(serialized_widget,indent=2)
            widget_file = open(os.path.join(widgets_directory,aw.uid+'.json'),'w')
            widget_file.write(widget_data)
            widget_file.close()

    if not global_change:
        writer.write("      No changes in the widgets detected!\n")

    writer.write("   > Exporting categories\n")

    global_change = False

    for category in categories:
        c = Category.objects.get(id=category)
        c.update_uid()
        data = serialize_category(c)
        
        created = True
        change = True
        try:
            category_file = open(os.path.join(categories_directory,c.uid+'.json'),'r')
            created = False
            c_data = json.loads(category_file.read())
            category_file.close()
            if c_data == data:
                change = False
        except:
            created = True
            change = True

        if change:
            global_change = True
            if created:
                writer.write("     + Exporting category "+str(c)+"\n")
            else:
                writer.write("     + Updating category "+str(c)+"\n")
            category_data = json.dumps(data,indent=2)
            category_file = open(os.path.join(categories_directory,c.uid+'.json'),'w')
            category_file.write(category_data)
            category_file.close()

    if not global_change:
        writer.write("      No changes in the categories detected!\n")




class Command(BaseCommand):
    args = 'package_name'
    help = 'Exports the package "package_name".'

    def handle(self, *args, **options):
        if (len(args) < 1):
            raise CommandError('Argument "package_name" is required.')

        package_name = args[0]
        writer = self.stdout

        export_package(package_name,writer)
        writer.write('Thanks for using the new export command. You rock.\n')