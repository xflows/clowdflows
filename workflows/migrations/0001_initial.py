# encoding: utf-8
import datetime
from south.db import db
from south.v2 import SchemaMigration
from django.db import models

class Migration(SchemaMigration):

    def forwards(self, orm):
        
        # Adding model 'Category'
        db.create_table('workflows_category', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('name', self.gf('django.db.models.fields.CharField')(max_length=50)),
            ('parent', self.gf('django.db.models.fields.related.ForeignKey')(blank=True, related_name='children', null=True, to=orm['workflows.Category'])),
            ('user', self.gf('django.db.models.fields.related.ForeignKey')(to=orm['auth.User'], null=True, blank=True)),
        ))
        db.send_create_signal('workflows', ['Category'])

        # Adding model 'Data'
        db.create_table('workflows_data', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('key', self.gf('django.db.models.fields.CharField')(max_length=200)),
            ('value', self.gf('django.db.models.fields.TextField')()),
        ))
        db.send_create_signal('workflows', ['Data'])

        # Adding model 'Workflow'
        db.create_table('workflows_workflow', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('name', self.gf('django.db.models.fields.CharField')(default='Untitled workflow', max_length=200)),
            ('user', self.gf('django.db.models.fields.related.ForeignKey')(related_name='workflows', to=orm['auth.User'])),
            ('widget', self.gf('django.db.models.fields.related.OneToOneField')(blank=True, related_name='workflow_link', unique=True, null=True, to=orm['workflows.Widget'])),
        ))
        db.send_create_signal('workflows', ['Workflow'])

        # Adding model 'AbstractWidget'
        db.create_table('workflows_abstractwidget', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('name', self.gf('django.db.models.fields.CharField')(max_length=200)),
            ('action', self.gf('django.db.models.fields.CharField')(max_length=200)),
            ('wsdl', self.gf('django.db.models.fields.URLField')(max_length=200, blank=True)),
            ('description', self.gf('django.db.models.fields.TextField')(blank=True)),
            ('category', self.gf('django.db.models.fields.related.ForeignKey')(related_name='widgets', to=orm['workflows.Category'])),
        ))
        db.send_create_signal('workflows', ['AbstractWidget'])

        # Adding model 'AbstractInput'
        db.create_table('workflows_abstractinput', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('name', self.gf('django.db.models.fields.CharField')(max_length=200)),
            ('short_name', self.gf('django.db.models.fields.CharField')(max_length=3)),
            ('description', self.gf('django.db.models.fields.TextField')(blank=True)),
            ('variable', self.gf('django.db.models.fields.CharField')(max_length=50)),
            ('widget', self.gf('django.db.models.fields.related.ForeignKey')(related_name='inputs', to=orm['workflows.AbstractWidget'])),
            ('required', self.gf('django.db.models.fields.BooleanField')(default=False)),
            ('parameter', self.gf('django.db.models.fields.BooleanField')(default=False)),
            ('default', self.gf('django.db.models.fields.TextField')(blank=True)),
            ('parameter_type', self.gf('django.db.models.fields.CharField')(max_length=50, null=True, blank=True)),
        ))
        db.send_create_signal('workflows', ['AbstractInput'])

        # Adding model 'AbstractOption'
        db.create_table('workflows_abstractoption', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('abstract_input', self.gf('django.db.models.fields.related.ForeignKey')(related_name='options', to=orm['workflows.AbstractInput'])),
            ('name', self.gf('django.db.models.fields.CharField')(max_length=200)),
            ('value', self.gf('django.db.models.fields.TextField')(blank=True)),
        ))
        db.send_create_signal('workflows', ['AbstractOption'])

        # Adding model 'AbstractOutput'
        db.create_table('workflows_abstractoutput', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('name', self.gf('django.db.models.fields.CharField')(max_length=200)),
            ('short_name', self.gf('django.db.models.fields.CharField')(max_length=3)),
            ('description', self.gf('django.db.models.fields.TextField')(blank=True)),
            ('variable', self.gf('django.db.models.fields.CharField')(max_length=50)),
            ('widget', self.gf('django.db.models.fields.related.ForeignKey')(related_name='outputs', to=orm['workflows.AbstractWidget'])),
        ))
        db.send_create_signal('workflows', ['AbstractOutput'])

        # Adding model 'Widget'
        db.create_table('workflows_widget', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('workflow', self.gf('django.db.models.fields.related.ForeignKey')(related_name='widgets', to=orm['workflows.Workflow'])),
            ('x', self.gf('django.db.models.fields.IntegerField')()),
            ('y', self.gf('django.db.models.fields.IntegerField')()),
            ('name', self.gf('django.db.models.fields.CharField')(max_length=200)),
            ('abstract_widget', self.gf('django.db.models.fields.related.ForeignKey')(blank=True, related_name='instances', null=True, to=orm['workflows.AbstractWidget'])),
        ))
        db.send_create_signal('workflows', ['Widget'])

        # Adding model 'Input'
        db.create_table('workflows_input', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('name', self.gf('django.db.models.fields.CharField')(max_length=200)),
            ('short_name', self.gf('django.db.models.fields.CharField')(max_length=3)),
            ('description', self.gf('django.db.models.fields.TextField')(null=True, blank=True)),
            ('variable', self.gf('django.db.models.fields.CharField')(max_length=50)),
            ('widget', self.gf('django.db.models.fields.related.ForeignKey')(related_name='inputs', to=orm['workflows.Widget'])),
            ('required', self.gf('django.db.models.fields.BooleanField')(default=False)),
            ('parameter', self.gf('django.db.models.fields.BooleanField')(default=False)),
            ('value', self.gf('django.db.models.fields.TextField')(null=True, blank=True)),
            ('inner_output', self.gf('django.db.models.fields.related.ForeignKey')(blank=True, related_name='outer_input_rel', null=True, to=orm['workflows.Output'])),
            ('outer_output', self.gf('django.db.models.fields.related.ForeignKey')(blank=True, related_name='inner_input_rel', null=True, to=orm['workflows.Output'])),
            ('parameter_type', self.gf('django.db.models.fields.CharField')(max_length=50, null=True, blank=True)),
        ))
        db.send_create_signal('workflows', ['Input'])

        # Adding model 'Option'
        db.create_table('workflows_option', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('input', self.gf('django.db.models.fields.related.ForeignKey')(related_name='options', to=orm['workflows.Input'])),
            ('name', self.gf('django.db.models.fields.CharField')(max_length=200)),
            ('value', self.gf('django.db.models.fields.TextField')(null=True, blank=True)),
        ))
        db.send_create_signal('workflows', ['Option'])

        # Adding model 'Output'
        db.create_table('workflows_output', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('name', self.gf('django.db.models.fields.CharField')(max_length=200)),
            ('short_name', self.gf('django.db.models.fields.CharField')(max_length=5)),
            ('description', self.gf('django.db.models.fields.TextField')(blank=True)),
            ('variable', self.gf('django.db.models.fields.CharField')(max_length=50)),
            ('widget', self.gf('django.db.models.fields.related.ForeignKey')(related_name='outputs', to=orm['workflows.Widget'])),
            ('inner_input', self.gf('django.db.models.fields.related.ForeignKey')(blank=True, related_name='outer_output_rel', null=True, to=orm['workflows.Input'])),
            ('outer_input', self.gf('django.db.models.fields.related.ForeignKey')(blank=True, related_name='inner_output_rel', null=True, to=orm['workflows.Input'])),
        ))
        db.send_create_signal('workflows', ['Output'])

        # Adding model 'Connection'
        db.create_table('workflows_connection', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('output', self.gf('django.db.models.fields.related.ForeignKey')(related_name='connections', to=orm['workflows.Output'])),
            ('input', self.gf('django.db.models.fields.related.ForeignKey')(related_name='connections', to=orm['workflows.Input'])),
            ('workflow', self.gf('django.db.models.fields.related.ForeignKey')(related_name='connections', to=orm['workflows.Workflow'])),
        ))
        db.send_create_signal('workflows', ['Connection'])

        # Adding model 'UserProfile'
        db.create_table('workflows_userprofile', (
            ('id', self.gf('django.db.models.fields.AutoField')(primary_key=True)),
            ('user', self.gf('django.db.models.fields.related.OneToOneField')(to=orm['auth.User'], unique=True)),
            ('active_workflow', self.gf('django.db.models.fields.related.ForeignKey')(blank=True, related_name='users', null=True, to=orm['workflows.Workflow'])),
        ))
        db.send_create_signal('workflows', ['UserProfile'])


    def backwards(self, orm):
        
        # Deleting model 'Category'
        db.delete_table('workflows_category')

        # Deleting model 'Data'
        db.delete_table('workflows_data')

        # Deleting model 'Workflow'
        db.delete_table('workflows_workflow')

        # Deleting model 'AbstractWidget'
        db.delete_table('workflows_abstractwidget')

        # Deleting model 'AbstractInput'
        db.delete_table('workflows_abstractinput')

        # Deleting model 'AbstractOption'
        db.delete_table('workflows_abstractoption')

        # Deleting model 'AbstractOutput'
        db.delete_table('workflows_abstractoutput')

        # Deleting model 'Widget'
        db.delete_table('workflows_widget')

        # Deleting model 'Input'
        db.delete_table('workflows_input')

        # Deleting model 'Option'
        db.delete_table('workflows_option')

        # Deleting model 'Output'
        db.delete_table('workflows_output')

        # Deleting model 'Connection'
        db.delete_table('workflows_connection')

        # Deleting model 'UserProfile'
        db.delete_table('workflows_userprofile')


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
            'value': ('django.db.models.fields.TextField', [], {'null': 'True', 'blank': 'True'}),
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
            'variable': ('django.db.models.fields.CharField', [], {'max_length': '50'}),
            'widget': ('django.db.models.fields.related.ForeignKey', [], {'related_name': "'outputs'", 'to': "orm['workflows.Widget']"})
        },
        'workflows.userprofile': {
            'Meta': {'object_name': 'UserProfile'},
            'active_workflow': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'users'", 'null': 'True', 'to': "orm['workflows.Workflow']"}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'user': ('django.db.models.fields.related.OneToOneField', [], {'to': "orm['auth.User']", 'unique': 'True'})
        },
        'workflows.widget': {
            'Meta': {'object_name': 'Widget'},
            'abstract_widget': ('django.db.models.fields.related.ForeignKey', [], {'blank': 'True', 'related_name': "'instances'", 'null': 'True', 'to': "orm['workflows.AbstractWidget']"}),
            'id': ('django.db.models.fields.AutoField', [], {'primary_key': 'True'}),
            'name': ('django.db.models.fields.CharField', [], {'max_length': '200'}),
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
