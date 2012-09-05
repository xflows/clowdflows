# encoding: utf-8
import datetime
from south.db import db
from south.v2 import SchemaMigration
from django.db import models

class Migration(SchemaMigration):

    def forwards(self, orm):
        
        # Adding field 'AbstractInput.multi'
        db.add_column('workflows_abstractinput', 'multi', self.gf('django.db.models.fields.BooleanField')(default=False), keep_default=False)


    def backwards(self, orm):
        
        # Deleting field 'AbstractInput.multi'
        db.delete_column('workflows_abstractinput', 'multi')


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
        'workflows.abstractinput': {
            'Meta': {'object_name': 'AbstractInput'},
            'default': ('django.db.models.fields.TextField', [], {'blank': 'True'}),
            'description': ('django.db.models.fields.TextField', [], {'blank': 'True'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'multi': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'parameter': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'parameter_type': ('django.db.models.fields.CharField', [], {'max_length': '50', 'null': 'True', 'blank': 'True'}),
            'required': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'short_name': ('django.db.models.fields.CharField', [], {'max_length': '3'}),
            'variable': ('django.db.models.fields.CharField', [], {'max_length': '50'}),
            'widget': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'inputs'", 'to': "orm['workflows.AbstractWidget']"})
        },
        'workflows.abstractoption': {
            'Meta': {'object_name': 'AbstractOption'},
            'abstract_input': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'options'", 'to': "orm['workflows.AbstractInput']"}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'value': ('django.db.models.fields.TextField', [], {'blank': 'True'})
        },
        'workflows.abstractoutput': {
            'Meta': {'object_name': 'AbstractOutput'},
            'description': ('django.db.models.fields.TextField', [], {'blank': 'True'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'short_name': ('django.db.models.fields.CharField', [], {'max_length': '3'}),
            'variable': ('django.db.models.fields.CharField', [], {'max_length': '50'}),
            'widget': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'outputs'", 'to': "orm['workflows.AbstractWidget']"})
        },
        'workflows.abstractwidget': {
            'Meta': {'object_name': 'AbstractWidget'},
            'action': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'category': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'widgets'", 'to': "orm['workflows.Category']"}),
            'description': ('django.db.models.fields.TextField', [], {'blank': 'True'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'visualization_template': ('django.db.models.fields.CharField', [], {'default': "''", 'max_length': '200', 'blank': 'True'}),
            'wsdl': ('django.db.models.fields.URLField', [], {'max_length': '200', 'blank': 'True'})
        },
        'workflows.category': {
            'Meta': {'ordering': "['name']", 'object_name': 'Category'},
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '50'}),
            'parent': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'children'", 'null': 'True', 'to': "orm['workflows.Category']"}),
            'user': ('django.db.models.fields.related.ForeignKey', [], {'to': "orm['auth.User']", 'null': 'True', 'blank': 'True'})
        },
        'workflows.connection': {
            'Meta': {'object_name': 'Connection'},
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'input': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'connections'", 'to': "orm['workflows.Input']"}),
            'output': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'connections'", 'to': "orm['workflows.Output']"}),
            'workflow': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'connections'", 'to': "orm['workflows.Workflow']"})
        },
        'workflows.data': {
            'Meta': {'object_name': 'Data'},
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'key': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'value': ('django.db.models.fields.TextField', [], {})
        },
        'workflows.input': {
            'Meta': {'object_name': 'Input'},
            'description': ('django.db.models.fields.TextField', [], {'null': 'True', 'blank': 'True'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'inner_output': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'outer_input_rel'", 'null': 'True', 'to': "orm['workflows.Output']"}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'outer_output': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'inner_input_rel'", 'null': 'True', 'to': "orm['workflows.Output']"}),
            'parameter': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'parameter_type': ('django.db.models.fields.CharField', [], {'max_length': '50', 'null': 'True', 'blank': 'True'}),
            'required': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'short_name': ('django.db.models.fields.CharField', [], {'max_length': '3'}),
            'value': ('picklefield.fields.PickledObjectField', [], {'null': 'True'}),
            'variable': ('django.db.models.fields.CharField', [], {'max_length': '50'}),
            'widget': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'inputs'", 'to': "orm['workflows.Widget']"})
        },
        'workflows.option': {
            'Meta': {'object_name': 'Option'},
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'input': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'options'", 'to': "orm['workflows.Input']"}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'value': ('django.db.models.fields.TextField', [], {'null': 'True', 'blank': 'True'})
        },
        'workflows.output': {
            'Meta': {'object_name': 'Output'},
            'description': ('django.db.models.fields.TextField', [], {'blank': 'True'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'inner_input': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'outer_output_rel'", 'null': 'True', 'to': "orm['workflows.Input']"}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'outer_input': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'inner_output_rel'", 'null': 'True', 'to': "orm['workflows.Input']"}),
            'short_name': ('django.db.models.fields.CharField', [], {'max_length': '5'}),
            'value': ('picklefield.fields.PickledObjectField', [], {'null': 'True'}),
            'variable': ('django.db.models.fields.CharField', [], {'max_length': '50'}),
            'widget': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'outputs'", 'to': "orm['workflows.Widget']"})
        },
        'workflows.userprofile': {
            'Meta': {'object_name': 'UserProfile'},
            'active_workflow': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'users'", 'null': 'True', 'to': "orm['workflows.Workflow']"}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'user': ('django.db.models.fields.related.OneToOneField', [], {'related_name': "'userprofile'", 'unique': 'True', 'to': "orm['auth.User']"})
        },
        'workflows.widget': {
            'Meta': {'object_name': 'Widget'},
            'abstract_widget': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'instances'", 'null': 'True', 'to': "orm['workflows.AbstractWidget']"}),
            'error': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'finished': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
            'running': ('django.db.models.fields.BooleanField', [], {'default': 'False'}),
            'workflow': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'widgets'", 'to': "orm['workflows.Workflow']"}),
            'x': ('django.db.models.fields.IntegerField', [], {}),
            'y': ('django.db.models.fields.IntegerField', [], {})
        },
        'workflows.workflow': {
            'Meta': {'object_name': 'Workflow'},
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'name': ('django.db.models.fields.CharField', [], {'default': "'Untitled workflow'", 'max_length': '200'}),
            'user': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'workflows'", 'to': "orm['auth.User']"}),
            'widget': ('django.db.models.fields.related.OneToOneField', [], {'blank': 'True', 'related_name': "'workflow_link'", 'unique': 'True', 'null': 'True', 'to': "orm['workflows.Widget']"})
        }
    }

    complete_apps = ['workflows']
