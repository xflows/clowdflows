# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
from django.conf import settings
import picklefield.fields


class Migration(migrations.Migration):

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
        ('workflows', '0001_initial'),
    ]

    operations = [
        migrations.CreateModel(
            name='Stream',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('last_executed', models.DateTimeField(auto_now_add=True)),
                ('period', models.IntegerField(default=60)),
                ('active', models.BooleanField(default=False)),
                ('user', models.ForeignKey(related_name='streams', to=settings.AUTH_USER_MODEL)),
                ('workflow', models.OneToOneField(related_name='stream', to='workflows.Workflow')),
            ],
        ),
        migrations.CreateModel(
            name='StreamWidgetData',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('value', picklefield.fields.PickledObjectField(null=True, editable=False)),
                ('stream', models.ForeignKey(related_name='widget_data', to='streams.Stream')),
                ('widget', models.ForeignKey(related_name='stream_data', to='workflows.Widget')),
            ],
        ),
    ]
