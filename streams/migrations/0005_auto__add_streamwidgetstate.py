# -*- coding: utf-8 -*-
import datetime
from south.db import db
from south.v2 import SchemaMigration
from django.db import models


class Migration(SchemaMigration):

    def forwards(self, orm):
        # Adding model 'StreamWidgetState'
        db.create_table('streams_streamwidgetstate', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('stream', self.gf('django.db.models.fields.related.ForeignKey')(related_name='widget_state', to=orm['streams.Stream'])),
            ('widget', self.gf('django.db.models.fields.related.ForeignKey')(related_name='stream_state', to=orm['workflows.Widget'])),
            ('state', self.gf('picklefield.fields.PickledObjectField')(null=True)),
        ))
        db.send_create_signal('streams', ['StreamWidgetState'])


    def backwards(self, orm):
        # Deleting model 'StreamWidgetState'
        db.delete_table('streams_streamwidgetstate')


    models = {
        'auth.group': {
            'Meta': {'object_name': 'Group'},
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'name': ('django.db.models.fields.CharField', [], {'unique': 'True', 'max_length': '80'}),
            'permissions': ('django.db.models.fields.related.ManyToManyField', [], {'to': "orm['auth.Permission']", 'symmetrical': 'False', 'blank': 'True'})
        },
        'auth.permission': {
            'Meta': {'ordering': "('content_type__app_label', 'content_type__model', 'codename')", 'unique_together': "(('content_type', 'codename'),)", 'object_name': 'Permission'},
            'codename': ('django.db.models.fields.CharField', [], {'max_length': '100'}),
            'content_type': ('django.db.models.fields.related.ForeignKey', [], {'to': "orm['contenttypes.ContentType']"}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '50'})
        },
        'auth.user': {
            'Meta': {'object_name': 'User'},
            'date_joined': ('django.db.models.fields.DateTimeField', [], {'default': 'datetime.datetime.now'}),
            'email': ('django.db.models.fields.EmailField', [], {'max_length': '75', 'blank': 'True'}),
            'first_name': ('django.db.models.fields.CharField', [], {'max_length': '30', 'blank': 'True'}),
            'groups': ('django.db.models.fields.related.ManyToManyField', [], {'to': "orm['auth.Group']", 'symmetrical': 'False', 'blank': 'True'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'is_active': ('django.db.models.fields.BooleanField', [], {'default': 'True'}),
            'is_staff': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'is_superuser': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'last_login': ('django.db.models.fields.DateTimeField', [], {'default': 'datetime.datetime.now'}),
            'last_name': ('django.db.models.fields.CharField', [], {'max_length': '30', 'blank': 'True'}),
            'password': ('django.db.models.fields.CharField', [], {'max_length': '128'}),
            'user_permissions': ('django.db.models.fields.related.ManyToManyField', [], {'to': "orm['auth.Permission']", 'symmetrical': 'False', 'blank': 'True'}),
            'username': ('django.db.models.fields.CharField', [], {'unique': 'True', 'max_length': '30'})
        },
        'contenttypes.contenttype': {
            'Meta': {'ordering': "('name',)", 'unique_together': "(('app_label', 'model'),)", 'object_name': 'ContentType', 'db_table': "'django_content_type'"},
            'app_label': ('django.db.models.fields.CharField', [], {'max_length': '100'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'model': ('django.db.models.fields.CharField', [], {'max_length': '100'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '100'})
        },
        'streams.stream': {
            'Meta': {'object_name': 'Stream'},
            'active': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'last_executed': ('django.db.models.fields.DateTimeField', [], {'auto_now_add': 'True', 'blank': 'True'}),
            'period': ('django.db.models.fields.IntegerField', [], {'default': '60'}),
            'user': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'streams'", 'to': "orm['auth.User']"}),
            'workflow': ('django.db.models.fields.related.OneToOneField', [], {'related_name': "'stream'", 'unique': 'True', 'to': "orm['workflows.Workflow']"})
        },
        'streams.streamwidgetdata': {
            'Meta': {'object_name': 'StreamWidgetData'},
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'stream': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'widget_data'", 'to': "orm['streams.Stream']"}),
            'value': ('picklefield.fields.PickledObjectField', [], {'null': 'True'}),
            'widget': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'stream_data'", 'to': "orm['workflows.Widget']"})
        },
        'streams.streamwidgetstate': {
            'Meta': {'object_name': 'StreamWidgetState'},
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'state': ('picklefield.fields.PickledObjectField', [], {'null': 'True'}),
            'stream': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'widget_state'", 'to': "orm['streams.Stream']"}),
            'widget': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'stream_state'", 'to': "orm['workflows.Widget']"})
        },
        'workflows.abstractwidget': {
            'Meta': {'ordering': "('order', 'name')", 'object_name': 'AbstractWidget'},
            'action': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'category': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'widgets'", 'to': "orm['workflows.Category']"}),
            'description': ('django.db.models.fields.TextField', [], {'blank': 'True'}),
            'has_progress_bar': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'image': ('workflows.thumbs.ThumbnailField', [], {'max_length': '100', 'null': 'True', 'blank': 'True'}),
            'interaction_view': ('django.db.models.fields.CharField', [], {'default': "''", 'max_length': '200', 'blank': 'True'}),
            'interactive': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'is_streaming': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'order': ('django.db.models.fields.PositiveIntegerField', [], {'default': '1'}),
            'package': ('django.db.models.fields.CharField', [], {'default': "''", 'max_length': '150', 'blank': 'True'}),
            'post_interact_action': ('django.db.models.fields.CharField', [], {'default': "''", 'max_length': '200', 'blank': 'True'}),
            'static_image': ('django.db.models.fields.CharField', [], {'default': "''", 'max_length': '250', 'blank': 'True'}),
            'streaming_visualization_view': ('django.db.models.fields.CharField', [], {'default': "''", 'max_length': '200', 'blank': 'True'}),
            'treeview_image': ('workflows.thumbs.ThumbnailField', [], {'max_length': '100', 'null': 'True', 'blank': 'True'}),
            'uid': ('django.db.models.fields.CharField', [], {'default': "''", 'max_length': '250', 'blank': 'True'}),
            'user': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'widgets'", 'null': 'True', 'to': "orm['auth.User']"}),
            'visualization_view': ('django.db.models.fields.CharField', [], {'default': "''", 'max_length': '200', 'blank': 'True'}),
            'windows_queue': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'wsdl': ('django.db.models.fields.URLField', [], {'max_length': '200', 'blank': 'True'}),
            'wsdl_method': ('django.db.models.fields.CharField', [], {'default': "''", 'max_length': '200', 'blank': 'True'})
        },
        'workflows.category': {
            'Meta': {'ordering': "('order', 'name')", 'object_name': 'Category'},
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '50'}),
            'order': ('django.db.models.fields.PositiveIntegerField', [], {'default': '1'}),
            'parent': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'children'", 'null': 'True', 'to': "orm['workflows.Category']"}),
            'uid': ('django.db.models.fields.CharField', [], {'default': "''", 'max_length': '250', 'blank': 'True'}),
            'user': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'categories'", 'null': 'True', 'to': "orm['auth.User']"}),
            'workflow': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'categories'", 'null': 'True', 'to': "orm['workflows.Workflow']"})
        },
        'workflows.widget': {
            'Meta': {'object_name': 'Widget'},
            'abstract_widget': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'instances'", 'null': 'True', 'to': "orm['workflows.AbstractWidget']"}),
            'error': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'finished': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'interaction_waiting': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'progress': ('django.db.models.fields.IntegerField', [], {'default': '0'}),
            'running': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'type': ('django.db.models.fields.CharField', [], {'default': "'regular'", 'max_length': '50'}),
            'workflow': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'widgets'", 'to': "orm['workflows.Workflow']"}),
            'x': ('django.db.models.fields.IntegerField', [], {}),
            'y': ('django.db.models.fields.IntegerField', [], {})
        },
        'workflows.workflow': {
            'Meta': {'ordering': "['name']", 'object_name': 'Workflow'},
            'description': ('django.db.models.fields.TextField', [], {'default': "''", 'blank': 'True'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'name': ('django.db.models.fields.CharField', [], {'default': "'Untitled workflow'", 'max_length': '200'}),
            'public': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'template_parent': ('django.db.models.fields.related.ForeignKey', [], {'default': 'None', 'to': "orm['workflows.Workflow']", 'null': 'True', 'on_delete': 'models.SET_NULL', 'blank': 'True'}),
            'user': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'workflows'", 'to': "orm['auth.User']"}),
            'widget': ('django.db.models.fields.related.OneToOneField', [], {'blank': 'True', 'related_name': "'workflow_link'", 'unique': 'True', 'null': 'True', 'to': "orm['workflows.Widget']"})
        }
    }

    complete_apps = ['streams']