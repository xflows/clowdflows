# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import workflows.thumbs
import django.db.models.deletion
from django.conf import settings
import picklefield.fields


class Migration(migrations.Migration):

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
    ]

    operations = [
        migrations.CreateModel(
            name='AbstractInput',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=200)),
                ('short_name', models.CharField(max_length=3)),
                ('description', models.TextField(blank=True)),
                ('variable', models.CharField(help_text=b'The variable attribute of both the input and the output are important because this is how the data will be accessed in the python function that is executed when the widget runs.', max_length=50)),
                ('required', models.BooleanField()),
                ('parameter', models.BooleanField()),
                ('multi', models.BooleanField(default=False, help_text=b'Inputs with this flag set will behave like this: whenever a connection is added to this input another input will be created on the fly that accepts the same data. In the action function, this will be represented as a list.')),
                ('default', models.TextField(blank=True)),
                ('parameter_type', models.CharField(blank=True, max_length=50, null=True, choices=[(b'text', b'Single line'), (b'password', b'Password'), (b'textarea', b'Multi line text'), (b'select', b'Select box'), (b'checkbox', b'Checkbox'), (b'file', b'File')])),
                ('order', models.PositiveIntegerField(default=1)),
                ('uid', models.CharField(default=b'', max_length=250, blank=True)),
            ],
            options={
                'ordering': ('order',),
            },
        ),
        migrations.CreateModel(
            name='AbstractOption',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=200)),
                ('value', models.TextField(blank=True)),
                ('uid', models.CharField(default=b'', max_length=250, blank=True)),
                ('abstract_input', models.ForeignKey(related_name='options', to='workflows.AbstractInput')),
            ],
            options={
                'ordering': ['name'],
            },
        ),
        migrations.CreateModel(
            name='AbstractOutput',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=200)),
                ('short_name', models.CharField(max_length=3)),
                ('description', models.TextField(blank=True)),
                ('variable', models.CharField(help_text=b'The variable attribute of both the input and the output are important because this is how the data will be accessed in the python function that is executed when the widget runs.', max_length=50)),
                ('order', models.PositiveIntegerField(default=1)),
                ('uid', models.CharField(default=b'', max_length=250, blank=True)),
            ],
            options={
                'ordering': ('order',),
            },
        ),
        migrations.CreateModel(
            name='AbstractWidget',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(help_text=b'Name is the name that will be displayed in the widget repository and under the actual widget itself.', max_length=200)),
                ('action', models.CharField(help_text=b'Action is the name of a python function that will be called when the widget is executed.', max_length=200)),
                ('wsdl', models.URLField(help_text=b'WSDL and WSDL method are used if the widget is a call of a Web Service. Web Service widgets are usually not entered in the admin panel, but in the application itself by importing a Web Service.', blank=True)),
                ('wsdl_method', models.CharField(default=b'', max_length=200, blank=True)),
                ('description', models.TextField(help_text=b'Description is used for a human readable description of what a widget does. A user will see this when he right clicks the widget and clicks help.', blank=True)),
                ('visualization_view', models.CharField(default=b'', help_text=b'Visualization view is (like the action) a python function that is a view that will render a template.', max_length=200, blank=True)),
                ('streaming_visualization_view', models.CharField(default=b'', help_text=b'Visualization view is (like the action) a python function that is a view that will render a template.', max_length=200, blank=True)),
                ('interactive', models.BooleanField(default=False, help_text=b'The widget can be interactive. This means that when a user executes the widget, the action will perform, then the interaction view will be executed and finally the Post interact action will be executed.')),
                ('interaction_view', models.CharField(default=b'', max_length=200, blank=True)),
                ('post_interact_action', models.CharField(default=b'', max_length=200, blank=True)),
                ('image', workflows.thumbs.ThumbnailField(help_text=b'Image and Treeview image are deprecated and will be phased out soon. Please use the static image field.', null=True, upload_to=b'images', blank=True)),
                ('treeview_image', workflows.thumbs.ThumbnailField(null=True, upload_to=b'treeview', blank=True)),
                ('static_image', models.CharField(default=b'', help_text=b'In the static image field just enter the filename of the image (without the path). The path will be $package_name$/icons/widget/$filename$ and $package_name$/icons/treeview/$filename$ where the treeview image is the small image that appears in the treeview on the left side and the widget image is the actual normal sized icon for the widget. IMPORTANT: the static image field only works if the package is set.', max_length=250, blank=True)),
                ('has_progress_bar', models.BooleanField(default=False, help_text=b'The flag has progress bar determines if the widget implements a progress bar.')),
                ('is_streaming', models.BooleanField(default=False, help_text=b'The is streaming flag is currently under construction, please do not use it yet.')),
                ('order', models.PositiveIntegerField(default=1, help_text=b'The Order determines the order in which the widget will be displayed in the repository. This is set automatically when sorting widgets in a single category from the admin.')),
                ('uid', models.CharField(default=b'', help_text=b'UID is set automatically when you export a package with the -u switch.', max_length=250, blank=True)),
                ('package', models.CharField(default=b'', help_text=b'Package is the package name. You are encouraged to use packages.', max_length=150, blank=True)),
                ('windows_queue', models.BooleanField(default=False, help_text=b"This is used for Matjaz Jursic's widgets.")),
            ],
            options={
                'ordering': ('order', 'name'),
            },
        ),
        migrations.CreateModel(
            name='Category',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=50)),
                ('order', models.PositiveIntegerField(default=1)),
                ('uid', models.CharField(default=b'', max_length=250, blank=True)),
                ('parent', models.ForeignKey(related_name='children', blank=True, to='workflows.Category', null=True)),
                ('user', models.ForeignKey(related_name='categories', blank=True, to=settings.AUTH_USER_MODEL, null=True)),
            ],
            options={
                'ordering': ('order', 'name'),
                'verbose_name_plural': 'categories',
            },
        ),
        migrations.CreateModel(
            name='Connection',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
            ],
        ),
        migrations.CreateModel(
            name='Input',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=200)),
                ('short_name', models.CharField(max_length=3)),
                ('description', models.TextField(null=True, blank=True)),
                ('variable', models.CharField(max_length=50)),
                ('required', models.BooleanField()),
                ('parameter', models.BooleanField()),
                ('value', picklefield.fields.PickledObjectField(null=True, editable=False)),
                ('multi_id', models.IntegerField(default=0)),
                ('parameter_type', models.CharField(blank=True, max_length=50, null=True, choices=[(b'text', b'Single line'), (b'textarea', b'Multi line text'), (b'select', b'Select box'), (b'file', b'File field'), (b'checkbox', b'Checkbox')])),
                ('order', models.PositiveIntegerField(default=1)),
            ],
            options={
                'ordering': ('order',),
            },
        ),
        migrations.CreateModel(
            name='Option',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=200)),
                ('value', models.TextField(null=True, blank=True)),
                ('input', models.ForeignKey(related_name='options', to='workflows.Input')),
            ],
            options={
                'ordering': ['name'],
            },
        ),
        migrations.CreateModel(
            name='Output',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=200)),
                ('short_name', models.CharField(max_length=5)),
                ('description', models.TextField(blank=True)),
                ('variable', models.CharField(max_length=50)),
                ('value', picklefield.fields.PickledObjectField(null=True, editable=False)),
                ('order', models.PositiveIntegerField(default=1)),
                ('inner_input', models.ForeignKey(related_name='outer_output_rel', blank=True, to='workflows.Input', null=True)),
                ('outer_input', models.ForeignKey(related_name='inner_output_rel', blank=True, to='workflows.Input', null=True)),
            ],
            options={
                'ordering': ('order',),
            },
        ),
        migrations.CreateModel(
            name='UserProfile',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
            ],
        ),
        migrations.CreateModel(
            name='Widget',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('x', models.IntegerField()),
                ('y', models.IntegerField()),
                ('name', models.CharField(max_length=200)),
                ('finished', models.BooleanField(default=False)),
                ('error', models.BooleanField(default=False)),
                ('running', models.BooleanField(default=False)),
                ('interaction_waiting', models.BooleanField(default=False)),
                ('type', models.CharField(default=b'regular', max_length=50, choices=[(b'regular', b'Regular widget'), (b'subprocess', b'Subprocess widget'), (b'input', b'Input widget'), (b'output', b'Output widget'), (b'for_input', b'For input'), (b'for_output', b'For output'), (b'cv_input', b'Cross Validation input'), (b'cv_output', b'Cross Validation output'), (b'cv_input2', b'Cross Validation input 2'), (b'cv_input3', b'Cross Validation input 3')])),
                ('progress', models.IntegerField(default=0)),
                ('abstract_widget', models.ForeignKey(related_name='instances', blank=True, to='workflows.AbstractWidget', null=True)),
            ],
        ),
        migrations.CreateModel(
            name='Workflow',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(default=b'Untitled workflow', max_length=200)),
                ('public', models.BooleanField(default=False)),
                ('description', models.TextField(default=b'', blank=True)),
                ('template_parent', models.ForeignKey(on_delete=django.db.models.deletion.SET_NULL, default=None, blank=True, to='workflows.Workflow', null=True)),
                ('user', models.ForeignKey(related_name='workflows', to=settings.AUTH_USER_MODEL)),
                ('widget', models.OneToOneField(related_name='workflow_link', null=True, blank=True, to='workflows.Widget')),
            ],
            options={
                'ordering': ['name'],
            },
        ),
        migrations.AddField(
            model_name='widget',
            name='workflow',
            field=models.ForeignKey(related_name='widgets', to='workflows.Workflow'),
        ),
        migrations.AddField(
            model_name='userprofile',
            name='active_workflow',
            field=models.ForeignKey(related_name='users', on_delete=django.db.models.deletion.SET_NULL, blank=True, to='workflows.Workflow', null=True),
        ),
        migrations.AddField(
            model_name='userprofile',
            name='user',
            field=models.OneToOneField(related_name='userprofile', to=settings.AUTH_USER_MODEL),
        ),
        migrations.AddField(
            model_name='output',
            name='widget',
            field=models.ForeignKey(related_name='outputs', to='workflows.Widget'),
        ),
        migrations.AddField(
            model_name='input',
            name='inner_output',
            field=models.ForeignKey(related_name='outer_input_rel', blank=True, to='workflows.Output', null=True),
        ),
        migrations.AddField(
            model_name='input',
            name='outer_output',
            field=models.ForeignKey(related_name='inner_input_rel', blank=True, to='workflows.Output', null=True),
        ),
        migrations.AddField(
            model_name='input',
            name='widget',
            field=models.ForeignKey(related_name='inputs', to='workflows.Widget'),
        ),
        migrations.AddField(
            model_name='connection',
            name='input',
            field=models.ForeignKey(related_name='connections', to='workflows.Input'),
        ),
        migrations.AddField(
            model_name='connection',
            name='output',
            field=models.ForeignKey(related_name='connections', to='workflows.Output'),
        ),
        migrations.AddField(
            model_name='connection',
            name='workflow',
            field=models.ForeignKey(related_name='connections', to='workflows.Workflow'),
        ),
        migrations.AddField(
            model_name='category',
            name='workflow',
            field=models.ForeignKey(related_name='categories', blank=True, to='workflows.Workflow', null=True),
        ),
        migrations.AddField(
            model_name='abstractwidget',
            name='category',
            field=models.ForeignKey(related_name='widgets', to='workflows.Category', help_text=b'Category determines to which category this widget belongs. Categories can be nested.'),
        ),
        migrations.AddField(
            model_name='abstractwidget',
            name='user',
            field=models.ForeignKey(related_name='widgets', blank=True, to=settings.AUTH_USER_MODEL, help_text=b'If the User field is blank, everyone will see the widget, otherwise just this user. This is mainly used for Web Service imports as they are only visible to users that imported them.', null=True),
        ),
        migrations.AddField(
            model_name='abstractoutput',
            name='widget',
            field=models.ForeignKey(related_name='outputs', to='workflows.AbstractWidget'),
        ),
        migrations.AddField(
            model_name='abstractinput',
            name='widget',
            field=models.ForeignKey(related_name='inputs', to='workflows.AbstractWidget'),
        ),
    ]
