"""
This file demonstrates writing tests using the unittest module. These will pass
when you run "manage.py test".

Replace this with more appropriate tests for your application.
"""

from django.test import TestCase
from workflows.engine import WorkflowRunner, WidgetRunner
from workflows.models import Workflow, Widget
import time

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