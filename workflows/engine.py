import workflows.library
import time

class WidgetRunner():
    def __init__(self,widget,workflow_runner):
        self.widget = widget
        self.inputs = widget.inputs.all()
        self.output = widget.outputs.all()
        self.input_data = {}
        self.output_data = {}
        self.workflow_runner = workflow_runner
        self.inner_workflow_runner = None
        if self.widget.abstract_widget is None:
            self.inner_workflow_runner = WorkflowRunner(self.widget.workflow_link,clean=True,parent=self.workflow_runner)

    def run(self):
        self.widget.running = True
        """ subprocesses and regular widgets get treated here """
        if self.widget.type == 'regular' or self.widget.type == 'subprocess':
            if not self.widget.abstract_widget is None:
                function_to_call = getattr(workflows.library,self.widget.abstract_widget.action)
                input_dict = self.get_input_dictionary()
                start = time.time()
                try:
                    if self.widget.abstract_widget:
                        if self.widget.abstract_widget.wsdl != '':
                            input_dict['wsdl']=self.abstract_widget.wsdl
                            input_dict['wsdl_method']=self.abstract_widget.wsdl_method
                        if self.widget.abstract_widget.has_progress_bar:
                            outputs = function_to_call(input_dict,self.widget)
                        elif self.widget.abstract_widget.is_streaming:
                            outputs = function_to_call(input_dict,self.widget,None)
                        else:
                            outputs = function_to_call(input_dict)
                    else:
                        """subworkflow"""
                        print "Subworkflow"
                except:
                    self.widget.error=True
                    self.widget.running=False
                    self.widget.finished=False
                    raise
                elapsed = (time.time()-start)
                outputs['clowdflows_elapsed']=elapsed
                self.assign_outputs(outputs)                    
        self.widget.running = False
        self.widget.finished = True

    def assign_outputs(self,outputs):
        for o in self.widget.outputs.all():
            if self.widget.abstract_widget:
                try:
                    o.value = outputs[o.variable]
                    c = self.workflow_runner.get_connection_for_output(o)
                    if c:
                        c.output.value = outputs[o.variable]
                except:
                    pass
            else:
                """if not self.workflow_link.is_for_loop():
                    o.value = o.inner_input_id
                    """
                print "not implemented"


    def get_input_dictionary(self):
        input_dictionary = {}
        for i in self.widget.inputs.all():
            """ if this isn't a parameter we need to fetch it
                from the output. """
            if not i.parameter:
                connection = self.workflow_runner.get_connection_for_input(i)
                if connection:
                    i.value = connection.output.value
                else:
                    i.value = None
            """ here we assign the value to the dictionary """
            if i.multi_id==0:
                input_dictionary[i.variable]=i.value
            else: # it's a multiple input
                if not i.variable in input_dictionary:
                    input_dictionary[i.variable]=[]
                if not i.value==None:
                    input_dictionary[i.variable].append(i.value)
        return input_dictionary

class WorkflowRunner():
    def __init__(self,workflow,clean=True,parent=None):
        self.workflow = workflow
        self.connections = workflow.connections.all().select_related('input','output')
        self.widgets = workflow.widgets.all().select_related('abstract_widget').prefetch_related('inputs','outputs')
        self.clean = clean

    def is_for_loop(self):
        for w in self.widgets:
            if w.type=='for_input':
                return True
        return False

    def is_cross_validation(self):
        for w in self.widgets:
            if w.type=='cv_input':
                return True
        return False

    def cleanup(self):
        for w in self.widgets:
            if self.clean:
                w.finished = False
            w.error = False        

    def get_connection_for_output(self,output):
        for c in self.connections:
            if c.output_id==output.id:
                return c
        return None

    def get_connection_for_input(self,input):
        for c in self.connections:
            if c.input_id==input.id:
                return c
        return None

    @property
    def finished_widgets(self):
        finished_widgets = []
        for w in self.widgets:
            if w.finished:
                finished_widgets.append(w)
        return finished_widgets

    @property
    def unfinished_widgets(self):
        unfinished_widgets = []
        for w in self.widgets:
            if not w.finished and not w.running and not w.error:
                unfinished_widgets.append(w)
        return unfinished_widgets

    @property
    def runnable_widgets(self):
        """ a widget is runnable if all widgets connected before
            it are finished (i.e. widgets that have outputs that 
            are connected to this widget's input) """
        finished_widget_ids = [w.id for w in self.finished_widgets]
        runnable = []
        for w in self.unfinished_widgets:
            ready_to_run = True
            for c in self.connections:
                if c.input.widget_id == w.id and not c.output.widget_id in finished_widget_ids:
                    ready_to_run = False
            if ready_to_run:
                runnable.append(w)
        return runnable

    def run(self):
        self.cleanup()
        runnable_widgets = self.runnable_widgets
        while len(runnable_widgets)>0:
            for w in runnable_widgets:
                wr = WidgetRunner(w,self)
                wr.run()
            runnable_widgets = self.runnable_widgets

    def save(self):
        for w in self.widgets:
            w.save()
            for i in w.inputs.all():
                i.save()
            for o in w.outputs.all():
                o.save()