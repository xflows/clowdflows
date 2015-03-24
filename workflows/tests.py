"""
This file demonstrates writing tests using the unittest module. These will pass
when you run "manage.py test".

Replace this with more appropriate tests for your application.
"""

from django.test import TestCase
from workflows.engine import WorkflowRunner, WidgetRunner
from workflows.models import Workflow, Widget
import time

class WorkflowExportTest(TestCase):
    fixtures = ['test_data',]
    def test_export_workflow(self):
        w = Workflow.objects.get(name='For loop test')
        a = w.export()
        b = {'widgets': [{'inputs': [{'name': u'Type your integer', 'short_name': u'int', 'inner_output': None, 'multi_id': 0, 'required': True, 'value': u'10', 'options': [], 'parameter_type': u'text', 'variable': u'integer', 'pk': 1, 'outer_output': None, 'parameter': True, 'order': 1, 'description': u''}], 'name': u'Create Integer', 'abstract_widget': u'8b20d4ad-c420-4156-973d-48d6d15934ba', 'workflow': None, 'outputs': [{'name': u'Integer', 'short_name': u'int', 'outer_input': None, 'variable': u'integer', 'pk': 1, 'inner_input': None, 'order': 1, 'description': u'The returned integer.'}], 'abstract_widget_package': u'base', 'y': 50, 'x': 50, 'type': u'regular'}, {'inputs': [{'name': u'Type your integer', 'short_name': u'int', 'inner_output': None, 'multi_id': 0, 'required': True, 'value': u'20', 'options': [], 'parameter_type': u'text', 'variable': u'integer', 'pk': 2, 'outer_output': None, 'parameter': True, 'order': 1, 'description': u''}], 'name': u'Create Integer', 'abstract_widget': u'8b20d4ad-c420-4156-973d-48d6d15934ba', 'workflow': None, 'outputs': [{'name': u'Integer', 'short_name': u'int', 'outer_input': None, 'variable': u'integer', 'pk': 2, 'inner_input': None, 'order': 1, 'description': u'The returned integer.'}], 'abstract_widget_package': u'base', 'y': 149, 'x': 50, 'type': u'regular'}, {'inputs': [{'name': u'Type your integer', 'short_name': u'int', 'inner_output': None, 'multi_id': 0, 'required': True, 'value': u'30', 'options': [], 'parameter_type': u'text', 'variable': u'integer', 'pk': 3, 'outer_output': None, 'parameter': True, 'order': 1, 'description': u''}], 'name': u'Create Integer', 'abstract_widget': u'8b20d4ad-c420-4156-973d-48d6d15934ba', 'workflow': None, 'outputs': [{'name': u'Integer', 'short_name': u'int', 'outer_input': None, 'variable': u'integer', 'pk': 3, 'inner_input': None, 'order': 1, 'description': u'The returned integer.'}], 'abstract_widget_package': u'base', 'y': 250, 'x': 50, 'type': u'regular'}, {'inputs': [{'name': u'Type your integer', 'short_name': u'int', 'inner_output': None, 'multi_id': 0, 'required': True, 'value': u'40', 'options': [], 'parameter_type': u'text', 'variable': u'integer', 'pk': 4, 'outer_output': None, 'parameter': True, 'order': 1, 'description': u''}], 'name': u'Create Integer', 'abstract_widget': u'8b20d4ad-c420-4156-973d-48d6d15934ba', 'workflow': None, 'outputs': [{'name': u'Integer', 'short_name': u'int', 'outer_input': None, 'variable': u'integer', 'pk': 4, 'inner_input': None, 'order': 1, 'description': u'The returned integer.'}], 'abstract_widget_package': u'base', 'y': 350, 'x': 50, 'type': u'regular'}, {'inputs': [{'name': u'Element', 'short_name': u'el', 'inner_output': None, 'multi_id': 44, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'list', 'pk': 5, 'outer_output': None, 'parameter': False, 'order': 1, 'description': u''}, {'name': u'Element', 'short_name': u'el', 'inner_output': None, 'multi_id': 44, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'list', 'pk': 6, 'outer_output': None, 'parameter': False, 'order': 1, 'description': u''}, {'name': u'Element', 'short_name': u'el', 'inner_output': None, 'multi_id': 44, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'list', 'pk': 7, 'outer_output': None, 'parameter': False, 'order': 1, 'description': u''}, {'name': u'Element', 'short_name': u'el', 'inner_output': None, 'multi_id': 44, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'list', 'pk': 8, 'outer_output': None, 'parameter': False, 'order': 1, 'description': u''}, {'name': u'Element', 'short_name': u'el', 'inner_output': None, 'multi_id': 44, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'list', 'pk': 9, 'outer_output': None, 'parameter': False, 'order': 1, 'description': u''}], 'name': u'Create List', 'abstract_widget': u'ed60cacd-633c-4c7c-b963-a5fda548bed4', 'workflow': None, 'outputs': [{'name': u'List', 'short_name': u'lst', 'outer_input': None, 'variable': u'list', 'pk': 5, 'inner_input': None, 'order': 1, 'description': u''}], 'abstract_widget_package': u'base', 'y': 157, 'x': 401, 'type': u'regular'}, {'inputs': [{'name': u'Input', 'short_name': u'inp', 'inner_output': 6, 'multi_id': 0, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'Input', 'pk': 10, 'outer_output': None, 'parameter': False, 'order': 1, 'description': None}], 'name': u'subprocess', 'abstract_widget': None, 'workflow': {'widgets': [{'inputs': [], 'name': u'Input', 'abstract_widget': None, 'workflow': None, 'outputs': [{'name': u'Input', 'short_name': u'inp', 'outer_input': 10, 'variable': u'Input', 'pk': 6, 'inner_input': None, 'order': 1, 'description': u''}], 'y': 50, 'x': 50, 'type': u'input'}, {'inputs': [{'name': u'Output', 'short_name': u'out', 'inner_output': None, 'multi_id': 0, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'Output', 'pk': 11, 'outer_output': 7, 'parameter': False, 'order': 1, 'description': None}], 'name': u'Output', 'abstract_widget': None, 'workflow': None, 'outputs': [], 'y': 55, 'x': 578, 'type': u'output'}, {'inputs': [{'name': u'Input', 'short_name': u'inp', 'inner_output': 8, 'multi_id': 0, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'Input', 'pk': 12, 'outer_output': None, 'parameter': False, 'order': 1, 'description': None}], 'name': u'Untitled widget', 'abstract_widget': None, 'workflow': {'widgets': [{'inputs': [], 'name': u'Input', 'abstract_widget': None, 'workflow': None, 'outputs': [{'name': u'Input', 'short_name': u'inp', 'outer_input': 12, 'variable': u'Input', 'pk': 8, 'inner_input': None, 'order': 1, 'description': u''}], 'y': 50, 'x': 50, 'type': u'input'}, {'inputs': [{'name': u'Output', 'short_name': u'out', 'inner_output': None, 'multi_id': 0, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'Output', 'pk': 13, 'outer_output': 9, 'parameter': False, 'order': 1, 'description': None}], 'name': u'Output', 'abstract_widget': None, 'workflow': None, 'outputs': [], 'y': 53, 'x': 534, 'type': u'output'}, {'inputs': [{'name': u'For input', 'short_name': u'for', 'inner_output': 10, 'multi_id': 0, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'For', 'pk': 14, 'outer_output': None, 'parameter': False, 'order': 1, 'description': None}], 'name': u'Untitled widget', 'abstract_widget': None, 'workflow': {'widgets': [{'inputs': [], 'name': u'For input', 'abstract_widget': None, 'workflow': None, 'outputs': [{'name': u'For input', 'short_name': u'for', 'outer_input': 14, 'variable': u'For', 'pk': 10, 'inner_input': None, 'order': 1, 'description': u''}], 'y': 50, 'x': 50, 'type': u'for_input'}, {'inputs': [{'name': u'For output', 'short_name': u'for', 'inner_output': None, 'multi_id': 0, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'For', 'pk': 15, 'outer_output': 11, 'parameter': False, 'order': 1, 'description': None}], 'name': u'For output', 'abstract_widget': None, 'workflow': None, 'outputs': [], 'y': 38, 'x': 396, 'type': u'for_output'}, {'inputs': [{'name': u'Integer 2', 'short_name': u'int', 'inner_output': None, 'multi_id': 0, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'integer2', 'pk': 16, 'outer_output': None, 'parameter': False, 'order': 1, 'description': u''}, {'name': u'Integer 1', 'short_name': u'int', 'inner_output': None, 'multi_id': 0, 'required': False, 'value': None, 'options': [], 'parameter_type': None, 'variable': u'integer1', 'pk': 17, 'outer_output': None, 'parameter': False, 'order': 2, 'description': u''}], 'name': u'Add integers', 'abstract_widget': u'e898b974-bafa-4b67-8918-b47b801e063e', 'workflow': None, 'outputs': [{'name': u'Integer', 'short_name': u'int', 'outer_input': None, 'variable': u'integer', 'pk': 12, 'inner_input': None, 'order': 1, 'description': u''}], 'abstract_widget_package': u'base', 'y': 35, 'x': 227, 'type': u'regular'}], 'connections': [{'output_id': 10, 'input_id': 16}, {'output_id': 10, 'input_id': 17}, {'output_id': 12, 'input_id': 15}], 'name': u'Untitled widget', 'description': u''}, 'outputs': [{'name': u'For output', 'short_name': u'for', 'outer_input': None, 'variable': u'For', 'pk': 11, 'inner_input': 15, 'order': 1, 'description': u''}], 'y': 51, 'x': 293, 'type': u'subprocess'}], 'connections': [{'output_id': 8, 'input_id': 14}, {'output_id': 11, 'input_id': 13}], 'name': u'Untitled widget', 'description': u''}, 'outputs': [{'name': u'Output', 'short_name': u'out', 'outer_input': None, 'variable': u'Output', 'pk': 9, 'inner_input': 13, 'order': 1, 'description': u''}], 'y': 56, 'x': 332, 'type': u'subprocess'}], 'connections': [{'output_id': 6, 'input_id': 12}, {'output_id': 9, 'input_id': 11}], 'name': u'subprocess', 'description': u''}, 'outputs': [{'name': u'Output', 'short_name': u'out', 'outer_input': None, 'variable': u'Output', 'pk': 7, 'inner_input': 11, 'order': 1, 'description': u''}], 'y': 157, 'x': 654, 'type': u'subprocess'}], 'connections': [{'output_id': 1, 'input_id': 5}, {'output_id': 2, 'input_id': 6}, {'output_id': 3, 'input_id': 7}, {'output_id': 4, 'input_id': 8}, {'output_id': 5, 'input_id': 10}], 'name': u'For loop test', 'description': u''}
        self.assertEqual(a,b)

class WorkflowEngineTest(TestCase):
    fixtures = ['test_data',]
    def test_fast_workflow_runner(self):
        w = Workflow.objects.get(name='For loop test')
        wr = WorkflowRunner(w)
        wr.run()
        wid = Widget.objects.get(id=6)
        o = wid.outputs.all()[0].value
        self.assertEqual(o,[20,40,60,80])

    def test_fast_workflow_runner_cv(self):
        w = Workflow.objects.get(name='Cross test')
        wr = WorkflowRunner(w)
        wr.run()
        wid = Widget.objects.get(id=16)
        o = wid.outputs.all()[0].value
        self.assertEqual(o,[[[[u'2'], [u'1']], [u'3'], 1],
                            [[[u'3'], [u'1']], [u'2'], 1],
                            [[[u'3'], [u'2']], [u'1'], 1]])

class WidgetEngineTest(TestCase):
    fixtures = ['test_data2',]
    def test_fast_widget_runner(self):
        w = Widget.objects.get(id=6)
        wr = WidgetRunner(w,workflow_runner=WorkflowRunner(w.workflow,clean=False),standalone=True)
        wr.run()
        wid = Widget.objects.get(id=6)
        o = wid.outputs.all()[0].value
        self.assertEqual(o,[20,40,60,80])

    def test_fast_widget_runner_cv(self):
        w = Widget.objects.get(id=16)
        wr = WidgetRunner(w,workflow_runner=WorkflowRunner(w.workflow,clean=False),standalone=True)
        wr.run()
        wid = Widget.objects.get(id=16)
        o = wid.outputs.all()[0].value
        self.assertEqual(o,[[[[u'2'], [u'1']], [u'3'], 1],
                            [[[u'3'], [u'1']], [u'2'], 1],
                            [[[u'3'], [u'2']], [u'1'], 1]])

