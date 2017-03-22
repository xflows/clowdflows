from django.db import models
from workflows.models import Workflow, Widget
from django.contrib.auth.models import User
import workflows.library

from picklefield.fields import PickledObjectField

class HaltStream(Exception):
    pass

# Create your models here.

class Stream(models.Model):
    user = models.ForeignKey(User,related_name="streams")
    workflow = models.OneToOneField(Workflow, related_name="stream")
    last_executed = models.DateTimeField(auto_now_add=True)
    period = models.IntegerField(default=60)
    active = models.BooleanField(default=False)

    @models.permalink
    def get_absolute_url(self):
        return ('stream', [str(self.id)])

    def stream_visualization_widgets(self):
        return self.workflow.widgets.all().exclude(abstract_widget__streaming_visualization_view='')

    def reset(self):
        self.widget_data.all().delete()

    def execute(self,workflow=None,outputs={}):
        if workflow is None:
            workflow = self.workflow
        ready_to_run = []
        widgets = workflow.widgets.all()

        #get unfinished
        if workflow.is_for_loop():
            fi = workflow.widgets.filter(type='for_input')[0]
            fo = workflow.widgets.filter(type='for_output')[0]
            outer_output = fo.inputs.all()[0].outer_output
            outputs[outer_output.pk]=(outer_output.variable,[])
            input_list = []
            try:
                if fi.outputs.all()[0].outer_input.connections.count() > 0:
                    input_list = outputs[fi.outputs.all()[0].outer_input.connections.all()[0].output.pk][1]
            except:
                    input_list = []
        else:
            input_list = [0]


        #print input_list

        for for_input in input_list:
            #print for_input
            finished = []
            unfinished_list = []
            halted = []
            loop = True
            while loop:
                for w in unfinished_list:
                    # prepare all the inputs for this widget
                    input_dict = {}
                    output_dict = {}
                    finish = True

                    for i in w.inputs.all():
                        #gremo pogledat ce obstaja povezava in ce obstaja gremo value prebrat iz outputa
                        if not i.parameter:
                            if i.connections.count() > 0:
                                #preberemo value iz output_dicta
                                i.value = outputs[i.connections.all()[0].output.pk][1]
                            else:
                                i.value = None
                        if i.multi_id == 0:
                            input_dict[i.variable]=i.value
                        else:
                            if not i.variable in input_dict:
                                input_dict[i.variable]=[]
                            if not i.value==None:
                                input_dict[i.variable].append(i.value)

                    if w.type == 'regular':
                        function_to_call = getattr(workflows.library,w.abstract_widget.action)
                        if w.abstract_widget.wsdl != '':
                            input_dict['wsdl']=w.abstract_widget.wsdl
                            input_dict['wsdl_method']=w.abstract_widget.wsdl_method
                    try:
                        if w.abstract_widget.has_progress_bar:
                            output_dict = function_to_call(input_dict,w)
                        elif w.abstract_widget.is_streaming:
                            output_dict = function_to_call(input_dict,w,self)
                        else:
                            output_dict = function_to_call(input_dict)
                    except HaltStream:
                        halted.append(w)
                        finish=False


                    if w.type == 'subprocess':
                        new_outputs = self.execute(workflow=w.workflow_link,outputs=outputs)
                        for o in w.outputs.all():
                            try:
                                outputs[o.pk]=new_outputs[o.pk]
                            except:
                                outputs[o.pk]=(o.variable,None)

                    if w.type == 'for_input':
                        for o in w.outputs.all():
                            outputs[o.pk]=(o.variable,for_input)
                            #print outputs[o.pk]
                            output_dict[o.variable]=for_input

                    if w.type == 'for_output':
                        for i in w.inputs.all():
                            outputs[i.outer_output.pk][1].append(input_dict[i.variable])
                            output_dict[i.variable]=input_dict[i.variable]

                    if w.type == 'input':
                        for o in w.outputs.all():
                            value = None
                            try:
                                if o.outer_input.connections.count() > 0:
                                    value = outputs[o.outer_input.connections.all()[0].output.pk][1]
                            except:
                                    value = None
                            output_dict[o.variable]=value

                    if finish:
                        if w.type == 'output':
                            for i in w.inputs.all():
                                outputs[i.outer_output.pk]=(i.outer_output.variable,input_dict[i.variable])


                        if w.type != 'subprocess':
                            for o in w.outputs.all():
                                outputs[o.pk]=(o.variable,output_dict[o.variable])

                        finished.append(w.pk)
                unfinished_list = []
                for w in widgets:
                    if not w.pk in finished:
                        ready_to_run = True
                        connections = workflow.connections.filter(input__widget=w)
                        for c in connections:
                            if c.output.widget.pk not in finished:
                                ready_to_run = False
                                break
                        if ready_to_run:
                            if w not in halted:
                                unfinished_list.append(w)
                if len(unfinished_list)==0:
                    loop = False
        return outputs


    def __unicode__(self):
        try:
            return unicode(self.workflow)+' stream'
        except:
            return 'Unknown stream'

class StreamWidgetData(models.Model):
    stream = models.ForeignKey(Stream, related_name="widget_data")
    widget = models.ForeignKey(Widget, related_name="stream_data")
    value = PickledObjectField(null=True)


