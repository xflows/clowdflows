from unicodedata import category
from django.core.management.base import BaseCommand, CommandError
from workflows.models import Category, AbstractWidget, AbstractInput, AbstractOutput, AbstractOption, Widget
from django.core import serializers
from optparse import make_option
import uuid
import os
import sys
import inspect
from django.conf import settings
import json
from .export_package import serialize_category, serialize_widget
from django.core.management.color import color_style

def parsewidgetdata(widget_data):
    widget = None
    inputs = []
    outputs = []
    options = []
    for i in widget_data:
        if i['model']=='workflows.abstractwidget':
            widget = i
        elif i['model']=='workflows.abstractinput':
            inputs.append(i)
        elif i['model']=='workflows.abstractoutput':
            outputs.append(i)
        elif i['model']=='workflows.abstractoption':
            options.append(i)
        else:
            raise CommandError("Wrong data in widget files!")
    return widget, inputs, outputs, options

def import_package(package_name,writer,external=False):
    style = color_style()
    if external:
        module = __import__(package_name)
        if '.' in package_name:
            cf_module = package_name.split('.')[-1]
            module = getattr(module, cf_module)
        package_directory = os.path.join(os.path.dirname(inspect.getfile(module)), 'package_data/')
    else:
        package_directory = os.path.join(os.path.dirname(os.path.realpath(__file__)),'../../'+package_name+"/package_data/")
    widgets_directory = os.path.join(package_directory,"widgets")
    deprecated_widgets_directory = os.path.join(package_directory,"deprecated_widgets")
    categories_directory = os.path.join(package_directory,"categories")        

    if not os.path.exists(package_directory) or not os.path.exists(widgets_directory) or not os.path.exists(categories_directory):
        raise CommandError("Cannot find package data. Are you sure this package has been exported already?")

    try:
        deprecated_widgets_files = os.listdir(deprecated_widgets_directory)
    except:
        deprecated_widgets_files = []
    widget_files = os.listdir(widgets_directory)
    category_files = os.listdir(categories_directory)

    writer.write(' > Importing categories\n')

    global_change = False

    for category_file in category_files:
        cfilepath = os.path.join(categories_directory,category_file)
        c_file = open(cfilepath,'r')
        c_data = json.loads(c_file.read())
        c_file.close()
        uid = c_data['fields']['uid']
        created = False
        try:
            c = Category.objects.get(uid=uid)
        except Category.DoesNotExist:
            created = True
            c = Category(uid=uid)
        old_c_data = serialize_category(c)
        if old_c_data != c_data:
            global_change = True
            if created:
                writer.write('   + Creating category '+str(c_data['fields']['name'])+'\n')
            else:
                writer.write('   + Updating category '+str(c_data['fields']['name'])+'\n')
            for field in c_data['fields'].keys():
                if field != 'parent':
                    setattr(c,field,c_data['fields'][field])
                else:
                    parent = None
                    if c_data['fields']['parent'] != None:
                        try:
                            parent = Category.objects.get(uid=c_data['fields']['parent'])
                        except Category.DoesNotExist:
                            parent = Category(uid=c_data['fields']['parent'],name="Temporary category name")
                            parent.save()
                    c.parent = parent
            c.save()

    if not global_change:
        #writer.write("    No changes detected in the categories.\n")
        pass

    global_change = False

    writer.write(' > Importing widgets\n')

    for widget_file in widget_files:
        wfilepath = os.path.join(widgets_directory,widget_file)
        w_file = open(wfilepath,'r')
        w_data = json.loads(w_file.read())
        w_file.close()
        widget, inputs, outputs, options = parsewidgetdata(w_data)
        created = False
        try:
            aw = AbstractWidget.objects.get(uid=widget['fields']['uid'],package=package_name)
        except AbstractWidget.DoesNotExist:
            aw = AbstractWidget(uid=widget['fields']['uid'],package=package_name)
            created = True
        if w_data != serialize_widget(aw):
            global_change = True
            if created:
                writer.write('   + Creating widget '+str(widget['fields']['name'])+'\n')
            else:
                writer.write('   + Updating widget '+str(widget['fields']['name'])+'\n')
            for field in widget['fields'].keys():
                if field != 'category':
                    setattr(aw,field,widget['fields'][field])
                else:
                    aw.category = Category.objects.get(uid=widget['fields']['category'])
            aw.save()
            for inp in inputs:
                try:
                    i = AbstractInput.objects.get(uid=inp['fields']['uid'])
                except AbstractInput.DoesNotExist:
                    i = AbstractInput(uid=inp['fields']['uid'])
                for field in inp['fields'].keys():
                    if field != 'widget':
                        setattr(i,field,inp['fields'][field])
                i.widget = aw
                i.save()
            # find stale inputs
            stale_ais = AbstractInput.objects.filter(widget=aw).exclude(uid__in=[inp['fields']['uid'] for inp in inputs])
            if stale_ais:
                stale_ais.delete()
                writer.write("     - Removing stale inputs\n")
            for out in outputs:
                try:
                    o = AbstractOutput.objects.get(uid=out['fields']['uid'])
                except AbstractOutput.DoesNotExist:
                    o = AbstractOutput(uid=out['fields']['uid'])
                for field in out['fields'].keys():
                    if field != 'widget':
                        setattr(o,field,out['fields'][field])
                o.widget = aw
                o.save()
            stale_aos = AbstractOutput.objects.filter(widget=aw).exclude(uid__in=[out['fields']['uid'] for out in outputs])
            if stale_aos:
                stale_aos.delete()
                writer.write("     - Removing stale outputs\n")
            for option in options:
                try:
                    o = AbstractOption.objects.get(uid=option['fields']['uid'])
                except AbstractOption.DoesNotExist:
                    o = AbstractOption(uid=option['fields']['uid'])
                for field in option['fields'].keys():
                    if field != 'abstract_input':
                        setattr(o,field,option['fields'][field])
                    else:
                        o.abstract_input = AbstractInput.objects.get(uid=option['fields']['abstract_input'])
                o.save()
            stale_os = AbstractOption.objects.filter(abstract_input__widget=aw).exclude(uid__in=[o['fields']['uid'] for o in options])
            if stale_os:
                stale_os.delete()
                writer.write("     - Removing stale options\n")

    if deprecated_widgets_files:
        for deprecated_widget_file in deprecated_widgets_files:
            wfilepath = os.path.join(deprecated_widgets_directory,deprecated_widget_file)
            w_file = open(wfilepath,'r')
            w_data = json.loads(w_file.read())
            w_file.close()
            widget, inputs, outputs, options = parsewidgetdata(w_data)
            created = False
            try:
                aw = AbstractWidget.objects.get(uid=widget['fields']['uid'],package=package_name)
                if Widget.objects.filter(abstract_widget=aw).count()==0:
                    writer.write('   - Removing widget '+str(widget['fields']['name'])+'\n')
                    aw.delete()
                else:
                    writer.write(style.ERROR('   - The widget '+str(widget['fields']['name'])+' is still used in workflows. It was not removed, but it is deprecated!\n'))
            except AbstractWidget.DoesNotExist:
                pass

    if not global_change:
        #writer.write("    No changes detected in the widgets.\n")
        pass



class Command(BaseCommand):
    args = 'package_name'
    help = 'Imports the package "package_name".'

    def handle(self, *args, **options):
        if (len(args) < 1):
            raise CommandError('Argument "package_name" is required.')

        package_name = args[0]
        external = package_name in settings.INSTALLED_APPS_EXTERNAL_PACKAGES

        if 'workflows.'+package_name not in settings.INSTALLED_APPS and not external:
            raise CommandError("Package not found in INSTALLED_APPS.")

        writer = self.stdout

        import_package(package_name,writer,external=external)

        writer.write('Thanks for using the new import command. You rock.\n')
