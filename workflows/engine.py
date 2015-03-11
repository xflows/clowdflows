class WidgetRunner():
    def __init__(self,widget,workflow_runner=None):
        self.widget = widget
        self.inputs = widget.inputs.all()
        self.output = widget.outputs.all()
        self.input_data = {}
        self.output_data = {}
        self.workflow_runner = workflow_runner

    def run(self):
        self.widget.finished=True

class WorkflowRunner():
    def __init__(self,workflow,clean=False):
        self.workflow = workflow
        self.connections = workflow.connections.all().select_related('input','output')
        self.widgets = workflow.widgets.all().prefetch_related('inputs','outputs')
        self.clean = clean
        if self.clean:
            for w in self.widgets:
                w.finished = False

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
            if not w.finished and not w.running:
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
        runnable_widgets = self.runnable_widgets
        while len(runnable_widgets)>0:
            for w in runnable_widgets:
                wr = WidgetRunner(w,self)
                wr.run()
            runnable_widgets = self.runnable_widgets