import workflows.library
import time
import random
from workflows.tasks import *

class WidgetRunner():
    def __init__(self,widget,workflow_runner,standalone=False):
        self.widget = widget
        self.inputs = widget.inputs.all()
        self.output = widget.outputs.all()
        self.workflow_runner = workflow_runner
        self.inner_workflow_runner = None
        self.standalone = standalone
        if self.standalone:
            for w in self.workflow_runner.widgets:
                if w.id == self.widget.id:
                    self.widget = w
                    break
            else:
                raise Exception("this shouldn't happen!")
    def run(self):
        self.widget.running = True
        """ subprocesses and regular widgets get treated here """
        if self.widget.type == 'regular' or self.widget.type == 'subprocess':
            if self.widget.abstract_widget:
                function_to_call = getattr(workflows.library,self.widget.abstract_widget.action)
            input_dict = self.get_input_dictionary()
            outputs = {}
            start = time.time()
            try:
                if self.widget.abstract_widget:
                    if self.widget.abstract_widget.wsdl != '':
                        input_dict['wsdl']=self.widget.abstract_widget.wsdl
                        input_dict['wsdl_method']=self.widget.abstract_widget.wsdl_method
                    if self.abstract_widget.windows_queue and settings.USE_WINDOWS_QUEUE:
                        if self.widget.abstract_widget.has_progress_bar:
                            outputs = executeWidgetFunction.apply_async([self.widget,input_dict],queue="windows").wait()
                        elif self.widget.abstract_widget.is_streaming:
                            outputs = executeWidgetProgressBar.apply_async([self.widget,input_dict],queue="windows").wait()
                        else:
                            outputs = executeWidgetStreaming.apply_async([self.widget,input_dict],queue="windows").wait()
                    else:
                        if self.widget.abstract_widget.has_progress_bar:
                            outputs = function_to_call(input_dict,self.widget)
                        elif self.widget.abstract_widget.is_streaming:
                            outputs = function_to_call(input_dict,self.widget,None)
                        else:
                            outputs = function_to_call(input_dict)
                else:
                    """ we run the subprocess """
                    self.inner_workflow_runner = WorkflowRunner(self.widget.workflow_link,parent=self.workflow_runner)
                    self.inner_workflow_runner.run()
            except:
                self.widget.error=True
                self.widget.running=False
                self.widget.finished=False
                raise
            elapsed = (time.time()-start)
            outputs['clowdflows_elapsed']=elapsed
            self.assign_outputs(outputs)
        elif self.widget.type == 'input':
            for o in self.widget.outputs.all():
                o.value = self.workflow_runner.parent.inputs[o.outer_input_id].value
        elif self.widget.type == 'output':
            input_dict = self.get_input_dictionary()
            for i in self.widget.inputs.all():
                self.workflow_runner.parent.outputs[i.outer_output_id].value = i.value
        elif self.widget.type == 'for_output':
            input_dict = self.get_input_dictionary()
            for i in self.widget.inputs.all():
                self.workflow_runner.parent.outputs[i.outer_output_id].value.append(i.value)
        elif self.widget.type == 'cv_output':
            input_dict = self.get_input_dictionary()
            for i in self.widget.inputs.all():
                self.workflow_runner.parent.outputs[i.outer_output_id].value.append(i.value)

        self.widget.running = False
        self.widget.error = False
        self.widget.finished = True
        if self.standalone:
            self.save()

    def assign_outputs(self,outputs):
        for o in self.widget.outputs.all():
            try:
                o.value = outputs[o.variable]
            except:
                pass

    def get_input_dictionary(self):
        input_dictionary = {}
        for i in self.widget.inputs.all():
            """ if this isn't a parameter we need to fetch it
                from the output. """
            if not i.parameter:
                connection = self.workflow_runner.get_connection_for_input(i)
                if connection:
                    i.value = self.workflow_runner.outputs[connection.output_id].value
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

    def save(self):
        for i in self.widget.inputs.all():
            i.save()
        for o in self.widget.outputs.all():
            o.save()
        self.widget.save()

class WorkflowRunner():
    def __init__(self,workflow,clean=True,parent=None):
        self.workflow = workflow
        self.connections = workflow.connections.all()
        self.widgets = workflow.widgets.all().select_related('abstract_widget').prefetch_related('inputs','outputs')
        self.inputs = {}
        self.outputs = {}
        for w in self.widgets:
            for i in w.inputs.all():
                self.inputs[i.id] = i
            for o in w.outputs.all():
                self.outputs[o.id] = o
        self.clean = clean
        self.parent = parent

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
                if self.inputs[c.input_id].widget_id == w.id and not self.outputs[c.output_id].widget_id in finished_widget_ids:
                    ready_to_run = False
            if ready_to_run:
                runnable.append(w)
        return runnable

    def run_all_unfinished_widgets(self):
        runnable_widgets = self.runnable_widgets
        while len(runnable_widgets)>0:
            for w in runnable_widgets:
                wr = WidgetRunner(w,self)
                try:
                    wr.run()
                except:
                    self.save()
                    raise
            runnable_widgets = self.runnable_widgets

    def run(self):
        self.cleanup()
        if self.is_for_loop():
            fi = None
            fo = None
            for w in self.widgets:
                if w.type=='for_input':
                    fi = w
                if w.type=='for_output':
                    fo = w
            outer_output = self.parent.outputs[fo.inputs.all()[0].outer_output_id]
            outer_output.value = []
            input_list = self.parent.inputs[fi.outputs.all()[0].outer_input_id].value
            for i in input_list:
                self.cleanup()
                proper_output = fi.outputs.all()[0]
                proper_output.value = i
                fi.finished = True
                self.run_all_unfinished_widgets()
        elif self.is_cross_validation():
            import random as rand
            fi = None
            fo = None
            for w in self.widgets:
                if w.type=='cv_input':
                    fi = w
                if w.type=='cv_output':
                    fo = w
            outer_output = self.parent.outputs[fo.inputs.all()[0].outer_output_id]
            outer_output.value = []
            input_list = self.parent.inputs[fi.outputs.all()[0].outer_input_id].value
            input_fold = self.parent.inputs[fi.outputs.all()[1].outer_input_id].value
            input_seed = self.parent.inputs[fi.outputs.all()[2].outer_input_id].value
            if input_fold != None:
                input_fold = int(input_fold)
            else:
                input_fold = 10

            if input_seed != None:
                input_seed = int(input_seed)
            else:
                input_seed = random.randint(0,10**9)

            input_type = input_list.__class__.__name__
            context = None
            if input_type == 'DBContext':
                context = input_list
                input_list = context.orng_tables.get(context.target_table,None)

            if not input_list:
                raise Exception('CrossValidation: Empty input list!')

            folds = []
            if hasattr(input_list, "get_items_ref"):
                import orange
                indices = orange.MakeRandomIndicesCV(input_list, randseed=input_seed, folds=input_fold, stratified=orange.MakeRandomIndices.Stratified)
                for i in range(input_fold):
                    output_train = input_list.select(indices, i, negate=1)
                    output_test = input_list.select(indices, i)
                    output_train.name = input_list.name
                    output_test.name = input_list.name
                    folds.append((output_train, output_test))
            else:
                rand.seed(input_seed)
                rand.shuffle(input_list)
                folds = [input_list[i::input_fold] for i in range(input_fold)]

            proper_output = fi.outputs.all()[2]
            proper_output.value = input_seed

            for i in range(len(folds)):
                #import pdb; pdb.set_trace()
                if hasattr(input_list, "get_items_ref"):
                    output_test = folds[i][1]
                    output_train = folds[i][0]
                else:
                    output_train = folds[:i] + folds[i+1:]
                    output_test = folds[i]
                if input_type == 'DBContext':
                    output_train_obj = context.copy()
                    output_train_obj.orng_tables[context.target_table] = output_train
                    output_test_obj = context.copy()
                    output_test_obj.orng_tables[context.target_table] = output_test
                    output_train = output_train_obj
                    output_test = output_test_obj

                self.cleanup()
                proper_output = fi.outputs.all()[0] # inner output
                proper_output.value = output_train
                proper_output = fi.outputs.all()[1] # inner output
                proper_output.value = output_test
                fi.finished=True # set the input widget as finished
                self.run_all_unfinished_widgets()
        else:
            self.run_all_unfinished_widgets()
        self.save()

    def save(self):
        for w in self.widgets:
            for i in w.inputs.all():
                i.save(force_update=True)
            for o in w.outputs.all():
                o.save(force_update=True)
            w.save(force_update=True)