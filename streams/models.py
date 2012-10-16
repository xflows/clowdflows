from django.db import models
from workflows.models import Workflow
from django.contrib.auth.models import User
import workflows.library

# Create your models here.

class Stream(models.Model):
    user = models.ForeignKey(User,related_name="streams")
    workflow = models.ForeignKey(Workflow, related_name="streams")
    last_executed = models.DateTimeField(auto_now_add=True)
    period = models.IntegerField(default=60)
    
    def execute(self,workflow=None,outputs={}):
        if workflow is None:
            workflow = self.workflow
        finished = []
        ready_to_run = []
        widgets = workflow.widgets.all()
        
        #get unfinished
        unfinished_list = []
        loop = True
        while loop:
            for w in unfinished_list:
                # prepare all the inputs for this widget
                input_dict = {}
                output_dict = {}                
                
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
                    if w.abstract_widget.has_progress_bar:
                        output_dict = function_to_call(input_dict,w)
                    else:
                        output_dict = function_to_call(input_dict)

                
                if w.type == 'subprocess':
                    new_outputs = self.execute(workflow=w.workflow_link,outputs=outputs)
                    for o in w.outputs.all():
                        value = None
                        try:
                            if o.inner_input.connections.count() > 0:
                                value = new_outputs[o.inner_input.connections.all()[0].output.pk][1]
                        except:
                                value = None
                        output_dict[o.variable]=value
                
                if w.type == 'for_input':
                    pass
                
                if w.type == 'for_output':
                    pass
                
                if w.type == 'input':
                    for o in w.outputs.all():
                        value = None
                        try:
                            if o.outer_input.connections.count() > 0:
                                value = outputs[o.outer_input.connections.all()[0].output.pk][1]
                        except:
                                value = None
                        output_dict[o.variable]=value
                
                if w.type == 'output':
                    pass
                    
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
                        unfinished_list.append(w)
            if len(unfinished_list)==0:
                loop = False
        return outputs
        
    
    def __unicode__(self):
        return unicode(self.workflow)+' stream'