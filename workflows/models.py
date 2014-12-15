from django.db import models
from django.contrib.auth.models import User
from django.db.models.signals import post_save
import workflows.library

import time

from picklefield.fields import PickledObjectField

from workflows.thumbs import ThumbnailField

from mothra.settings import DEBUG
from mothra.settings import USE_CONCURRENCY

if USE_CONCURRENCY:
    from workflows.tasks import runWidgetAsync, runForLoopIteration

from workflows.tasks import executeWidgetFunction, executeWidgetProgressBar, executeWidgetStreaming, executeWidgetWithRequest, runWidget, executeWidgetPostInteract

class WidgetException(Exception):
    pass

class Connection(models.Model):
    output = models.ForeignKey("Output",related_name="connections")
    input = models.ForeignKey("Input",related_name="connections")
    workflow = models.ForeignKey("Workflow",related_name="connections")

class Category(models.Model):
    name = models.CharField(max_length=50)
    parent = models.ForeignKey('self',related_name="children",null=True,blank=True)
    user = models.ForeignKey(User,null=True,blank=True,related_name="categories")

    workflow = models.ForeignKey('Workflow',null=True,blank=True,related_name="categories")

    order = models.PositiveIntegerField(default=1)

    uid = models.CharField(max_length=250,blank=True,default='')

    class Meta:
        verbose_name_plural = "categories"
        ordering = ('order','name',)

    def __unicode__(self):
        if self.parent is None:
            return unicode(self.name)
        else:
            return unicode(unicode(self.parent)+" :: "+self.name)

class Workflow(models.Model):
    name = models.CharField(max_length=200,default='Untitled workflow') # a field
    user = models.ForeignKey(User,related_name="workflows") # django relationship (ForeignKey), each Workflow is related to a single User
    public = models.BooleanField(default=False) # a field
    description = models.TextField(blank=True,default='') # a field
    widget = models.OneToOneField('Widget',related_name="workflow_link",blank=True,null=True)
    template_parent = models.ForeignKey('Workflow',blank=True,null=True,default=None,on_delete=models.SET_NULL)

    def can_be_streaming(self):
        """ Method checks if workflow can be streamed. Check if there is at least one widget with
        the flag abstract_widget__is_streaming on True.  """
        if self.widgets.filter(abstract_widget__is_streaming=True).count()>0:
            return True
        else:
            return False

    def is_for_loop(self):
        """ Method checks if workflow is a for loop. Checks if at least one widget is 
        type for_input. """
        if self.widgets.filter(type='for_input').count()>0:
            return True
        else:
            return False

    def is_cross_validation(self):
        """ Method checks if workflow is a for loop. Checks if at least one widget is 
        type cv input. """
        if self.widgets.filter(type='cv_input').count()>0:
            return True
        else:
            return False

    def get_ready_to_run(self):
        """ Method prepares this workflows widgets. Returns a list of widget id-s. """
        widgets = self.widgets.all()
        unfinished_list = []
        for w in widgets:
            if not w.finished and not w.running:
                """ if widget isn't finished and is not running than true"""
                ready_to_run = True
                connections = self.connections.filter(input__widget=w)
                for c in connections:
                    if not c.output.widget.finished:
                        """ if widget not finished than true """
                        ready_to_run = False
                        break
                if ready_to_run:
                    unfinished_list.append(w.id)
        return unfinished_list

    def get_runnable_widgets(self):
        """ Method is the same as get_ready_to_run method. The difference is only that this method
        returns a list widgets as objects (and not only id-s).  """
        widgets = self.widgets.all()
        unfinished_list = []
        for w in widgets:
            if not w.finished and not w.running:
                """ if widget isn't finished and is not running than true"""
                ready_to_run = True
                connections = self.connections.filter(input__widget=w)
                for c in connections:
                    if not c.output.widget.finished:
                        ready_to_run = False
                        break
                if ready_to_run:
                    unfinished_list.append(w)
        return unfinished_list

    def run_for_loop(self):
        """ Method runs the workflow for loop. The use of [0] at the end of lines is because
        there can be only one for loop in one workflow. This way we take the first one. """
        #clear for_input and for_output
        print("run_for_loop")
        fi = self.widgets.filter(type='for_input')[0]
        fo = self.widgets.filter(type='for_output')[0]
        outer_output = fo.inputs.all()[0].outer_output
        outer_output.value=[]
        outer_output.save()

        input_list = fi.outputs.all()[0].outer_input.value # get all inputs from outer part
        progress_total = len(input_list) # for progress bar
        current_iteration = 0
        for i in input_list:
            print(i);
            """ Different parameters on which the widgets are going to be run"""
            fi.unfinish() # resets widgets, (read all widgets.finished=false)
            fo.unfinish() # resets widgets, (read all widgets.finished=false)
            proper_output = fi.outputs.all()[0] # inner output
            proper_output.value = i
            proper_output.save()
            fi.finished=True # set the input widget as finished
            fi.save()
            if not USE_CONCURRENCY or 1==1:
                """ This if statement is always true. """
                unfinished_list = self.get_runnable_widgets()
                try:
                    while len(unfinished_list)>0:
                        for w in unfinished_list:
                            w.run(True) # run the widget
                            total = self.widgets.count()
                            completed = self.widgets.filter(finished=True).count()
                            self.widget.progress = (int)((current_iteration*100.0/progress_total)+(((completed*1.0)/total)*(100/progress_total)))
                            self.widget.save()
                        unfinished_list = self.get_runnable_widgets()
                except:
                    raise
            else:
                """ This part is never executed  """
                unfinished_list = self.get_runnable_widgets()
                try:
                    statuses = {}
                    total = self.widgets.count()
                    completed = self.widgets.filter(finished=True).count()
                    while len(unfinished_list)>0:
                        for w in unfinished_list:
                            if statuses.has_key(w.pk):
                                if statuses[w.pk].failed():
                                    raise Exception(statuses[w.pk].info[0])
                            else:
                                statuses[w.pk]=runWidgetAsync.delay(w)
                            completed = self.widgets.filter(finished=True).count()
                            if self.widget:
                                self.widget.progress = (int)((current_iteration*100.0/progress_total)+(((completed*1.0)/total)*(100/progress_total)))
                                self.widget.save()
                        is_running=True
                        unfinished_list = self.get_runnable_widgets()
                        while len(unfinished_list)==0 and is_running:
                            unfinished_list = self.get_runnable_widgets()
                            is_running = False
                            for st in statuses.values():
                                if st.status == 'PENDING':
                                    is_running = True
                                else:
                                    st.get()
                                    completed = self.widgets.filter(finished=True).count()
                                    if self.widget:
                                        self.widget.progress = (int)((current_iteration*100.0/progress_total)+(((completed*1.0)/total)*(100/progress_total)))
                                        self.widget.save()
                            unfinished_list = self.get_runnable_widgets()
                except:
                    raise
            current_iteration = current_iteration+1

    def run_cross_validation(self):
        """ Method runs cross_validation. """
        #clear for_input and for_output
        print("run_cross_validation")
        import random as rand
        fi = self.widgets.filter(type='cv_input')[0]
        fo = self.widgets.filter(type='cv_output')[0]
        outer_output = fo.inputs.all()[0].outer_output
        outer_output.value=[]
        outer_output.save()

        # get all inputs from outer part
        input_list = fi.outputs.all()[0].outer_input.value 
        input_fold = fi.outputs.all()[1].outer_input.value
        input_seed = fi.outputs.all()[2].outer_input.value

        if input_fold != None:
            #check if we have an input
            input_fold = int(fi.outputs.all()[1].outer_input.value)
        else:
            input_fold = 10
        
        if input_seed != None:
            #check if we have an input
            input_seed = int(fi.outputs.all()[2].outer_input.value)
        else:
            input_seed = 0

        # Special case when reading from a DB
        input_type = input_list.__class__.__name__
        context = None
        if input_type == 'DBContext':
            context = input_list
            input_list = context.orng_tables.get(context.target_table, None)

        if not input_list:
            raise Exception('CrossValidation: Empty input list!')

        progress_total = len(input_list) # for progress bar
        current_iteration = 0

        # create folds

        folds = []
        if hasattr(input_list, "get_items_ref"):
            import orange 
            # Orange table on input, so we cannot do slices
            indices = orange.MakeRandomIndicesCV(input_list, randseed=input_seed)
            for i in range(input_fold):
                output_train = input_list.select(indices, i, negate=1)
                output_test = input_list.select(indices, i)
                folds.append((output_train, output_test))
        else:
            rand.seed(input_seed)
            rand.shuffle(input_list)
            folds = [input_list[i::input_fold] for i in range(input_fold)]

        # pass forward the seed
        proper_output = fi.outputs.all()[2] # inner output
        proper_output.value = input_seed
        proper_output.save()

        # this for loop delets all previous results
        for i in fo.inputs.all():
            if not i.parameter:
                if i.connections.count() > 0:
                    i.value = []
                    i.save()

        for i in range(len(folds)):
            #import pdb; pdb.set_trace()
            if hasattr(input_list, "get_items_ref"):
                output_test = folds[i][0]
                output_train = folds[i][1]
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

            """ Different parameters on which the widgets are going to be run"""
            fi.unfinish() # resets widgets, (read all widgets.finished=false)
            fo.unfinish() # resets widgets, (read all widgets.finished=false)
            proper_output = fi.outputs.all()[0] # inner output
            proper_output.value = output_train
            proper_output.save()
            proper_output = fi.outputs.all()[1] # inner output
            proper_output.value = output_test
            proper_output.save()
            fi.finished=True # set the input widget as finished
            fi.save()
            if not USE_CONCURRENCY or 1==1:
                """ This if statement is always true. """
                unfinished_list = self.get_runnable_widgets()
                try:
                    while len(unfinished_list)>0:
                        for w in unfinished_list:
                            w.run(True) # run the widget
                            total = self.widgets.count()
                            completed = self.widgets.filter(finished=True).count()
                            self.widget.progress = (int)((current_iteration*100.0/progress_total)+(((completed*1.0)/total)*(100/progress_total)))
                            self.widget.save()
                        unfinished_list = self.get_runnable_widgets()
                except:
                    raise
            current_iteration = current_iteration+1

    def run(self):
        if not USE_CONCURRENCY or not self.widget:
            unfinished_list = self.get_runnable_widgets()
            try:
                total = self.widgets.count()
                completed = self.widgets.filter(finished=True).count()
                while len(unfinished_list)>0:
                    for w in unfinished_list:
                        w.run(True)
                        #runWidgetAsync.delay(w)
                        completed = self.widgets.filter(finished=True).count()
                        if self.widget:
                            self.widget.progress = (int)(((completed*1.0)/total)*100)
                            self.widget.save()
                    unfinished_list = self.get_runnable_widgets()
            except:
                raise
        else:
            unfinished_list = self.get_runnable_widgets()
            try:
                statuses = {}
                total = self.widgets.count()
                completed = self.widgets.filter(finished=True).count()
                while len(unfinished_list)>0:
                    for w in unfinished_list:
                        if statuses.has_key(w.pk):
                            if statuses[w.pk].failed():
                                raise Exception(statuses[w.pk].info[0])
                        else:
                            statuses[w.pk]=runWidgetAsync.delay(w)
                        completed = self.widgets.filter(finished=True).count()
                        if self.widget:
                            self.widget.progress = (int)(((completed*1.0)/total)*100)
                            self.widget.save()
                    is_running=True
                    unfinished_list = self.get_runnable_widgets()
                    while len(unfinished_list)==0 and is_running:
                        unfinished_list = self.get_runnable_widgets()
                        is_running = False
                        for st in statuses.values():
                            if st.status == 'PENDING':
                                is_running = True
                            else:
                                st.get()
                                completed = self.widgets.filter(finished=True).count()
                                if self.widget:
                                    self.widget.progress = (int)(((completed*1.0)/total)*100)
                                    self.widget.save()
                        time.sleep(1)
                    unfinished_list = self.get_runnable_widgets()
            except:
                raise
    def rename(self,new_name):
        self.name = new_name
        self.save()

    @models.permalink
    def get_absolute_url(self):
        return ('open workflow', [str(self.id)])

    @models.permalink
    def get_copy_url(self):
        return ('copy workflow', [str(self.id)])

    @models.permalink
    def get_info_url(self):
        return ('workflow information', [str(self.id)])

    def __unicode__(self):
        return unicode(self.name)

    class Meta:
        ordering = ['name']

class AbstractWidget(models.Model):
    name = models.CharField(max_length=200,help_text='Name is the name that will be displayed in the widget repository and under the actual widget itself.')
    action = models.CharField(max_length=200,help_text='Action is the name of a python function that will be called when the widget is executed.')
    wsdl = models.URLField(max_length=200,blank=True,help_text='WSDL and WSDL method are used if the widget is a call of a Web Service. Web Service widgets are usually not entered in the admin panel, but in the application itself by importing a Web Service.')
    wsdl_method = models.CharField(max_length=200,blank=True,default='')
    description = models.TextField(blank=True,help_text='Description is used for a human readable description of what a widget does. A user will see this when he right clicks the widget and clicks help.')
    category = models.ForeignKey(Category,related_name="widgets",help_text='Category determines to which category this widget belongs. Categories can be nested.')
    visualization_view = models.CharField(max_length=200,blank=True,default='',help_text='Visualization view is (like the action) a python function that is a view that will render a template.')
    streaming_visualization_view = models.CharField(max_length=200,blank=True,default='',help_text='Visualization view is (like the action) a python function that is a view that will render a template.')
    user = models.ForeignKey(User,blank=True,null=True,related_name="widgets",help_text='If the User field is blank, everyone will see the widget, otherwise just this user. This is mainly used for Web Service imports as they are only visible to users that imported them.')
    interactive = models.BooleanField(default=False,help_text='The widget can be interactive. This means that when a user executes the widget, the action will perform, then the interaction view will be executed and finally the Post interact action will be executed.')
    interaction_view = models.CharField(max_length=200,blank=True,default='')
    post_interact_action = models.CharField(max_length=200,blank=True,default='')

    image = ThumbnailField(blank=True,null=True,upload_to="images",size=(34,34),help_text='Image and Treeview image are deprecated and will be phased out soon. Please use the static image field.')
    treeview_image = ThumbnailField(blank=True,null=True,upload_to="treeview",size=(16,16))

    static_image = models.CharField(max_length=250,blank=True,default='',help_text='In the static image field just enter the filename of the image (without the path). The path will be $package_name$/icons/widget/$filename$ and $package_name$/icons/treeview/$filename$ where the treeview image is the small image that appears in the treeview on the left side and the widget image is the actual normal sized icon for the widget. IMPORTANT: the static image field only works if the package is set.')

    has_progress_bar = models.BooleanField(default=False,help_text='The flag has progress bar determines if the widget implements a progress bar.')
    is_streaming = models.BooleanField(default=False,help_text='The is streaming flag is currently under construction, please do not use it yet.')

    order = models.PositiveIntegerField(default=1,help_text='The Order determines the order in which the widget will be displayed in the repository. This is set automatically when sorting widgets in a single category from the admin.')

    uid = models.CharField(max_length=250,blank=True,default='',help_text='UID is set automatically when you export a package with the -u switch.')

    package = models.CharField(max_length=150,blank=True,default='',help_text='Package is the package name. You are encouraged to use packages.')

    windows_queue = models.BooleanField(default=False,help_text="This is used for Matjaz Jursic's widgets.")

    class Meta:
        ordering = ('order','name',)

    def set_uid(self,commit=False):
        import uuid
        self.uid = uuid.uuid4()
        if commit:
            self.save()
        for i in self.inputs.all():
            i.uid = uuid.uuid4()
            if commit:
                i.save()
            for option in i.options.all():
                option.uid = uuid.uuid4()
                if commit:
                    option.save()
        for o in self.outputs.all():
            o.uid = uuid.uuid4()
            if commit:
                o.save()

    def __unicode__(self):
        return unicode(self.name)

class AbstractInput(models.Model):
    name = models.CharField(max_length=200)
    short_name = models.CharField(max_length=3)
    description = models.TextField(blank=True)
    variable = models.CharField(max_length=50,help_text='The variable attribute of both the input and the output are important because this is how the data will be accessed in the python function that is executed when the widget runs.')
    widget = models.ForeignKey(AbstractWidget,related_name="inputs")
    required = models.BooleanField()
    parameter = models.BooleanField()
    multi = models.BooleanField(default=False,help_text='Inputs with this flag set will behave like this: whenever a connection is added to this input another input will be created on the fly that accepts the same data. In the action function, this will be represented as a list.')
    default = models.TextField(blank=True)
    PARAMETER_CHOICES = (
        ('text','Single line'),
        ('password', 'Password'),
        ('textarea','Multi line text'),
        ('select', 'Select box'),
        ('checkbox', 'Checkbox'),
        ('file', 'File'),
    )
    parameter_type = models.CharField(max_length=50,choices=PARAMETER_CHOICES,blank=True,null=True)

    order = models.PositiveIntegerField(default=1)

    uid = models.CharField(max_length=250,blank=True,default='')

    def __unicode__(self):
        return unicode(self.name)

    class Meta:
        ordering = ('order',)

class AbstractOption(models.Model):
    abstract_input = models.ForeignKey(AbstractInput,related_name="options")
    name = models.CharField(max_length=200)
    value = models.TextField(blank=True)

    uid = models.CharField(max_length=250,blank=True,default='')

    def __unicode__(self):
        return unicode(self.name)

    class Meta:
        ordering = ['name']

class AbstractOutput(models.Model):
    name = models.CharField(max_length=200)
    short_name = models.CharField(max_length=3)
    description = models.TextField(blank=True)
    variable = models.CharField(max_length=50,help_text='The variable attribute of both the input and the output are important because this is how the data will be accessed in the python function that is executed when the widget runs.')
    widget = models.ForeignKey(AbstractWidget,related_name="outputs")

    order = models.PositiveIntegerField(default=1)

    uid = models.CharField(max_length=250,blank=True,default='')

    class Meta:
        ordering = ('order',)

    def __unicode__(self):
        return unicode(self.name)

class Widget(models.Model):
    """ Widget """
    # django relationship (ForeignKey), each widget is related to a single workflow
    workflow = models.ForeignKey(Workflow,related_name="widgets")
    x = models.IntegerField() # a field
    y = models.IntegerField() # a field
    name = models.CharField(max_length=200) # a field
    abstract_widget = models.ForeignKey(AbstractWidget,related_name="instances",blank=True,null=True)
    finished = models.BooleanField(default=False) # a field
    error = models.BooleanField(default=False) # a field
    running = models.BooleanField(default=False) # a field
    interaction_waiting = models.BooleanField(default=False) # a field
    """ type of widgets """
    WIDGET_CHOICES = (
        ('regular','Regular widget'),
        ('subprocess','Subprocess widget'),
        ('input', 'Input widget'),
        ('output', 'Output widget'),
    )
    type = models.CharField(max_length=50,choices=WIDGET_CHOICES,default='regular')

    progress = models.IntegerField(default=0)

    def is_visualization(self):
        try:
            if self.abstract_widget.visualization_view != '':
                return True
        except:
            return False

    def ready_to_run(self):
        cons = Connection.objects.filter(input__widget=self)
        for c in cons:
            if not c.output.widget.finished:
                return False
        return True

    def unfinish(self):
        self.reset_descendants()

    def subunfinish(self):
        if self.type == 'subprocess':
            for w in self.workflow_link.widgets.all():
                w.finished=False
                w.error = False
                w.save()
                if w.type=='subprocess':
                    w.subunfinish()

    def rename(self,new_name):
        self.name = new_name
        self.save()
        if self.type=='input':
            inp = self.outputs.all()[0]
            inp.short_name = self.name[:3]
            inp.name = self.name
            inp.save()
            inp.outer_input.name = self.name
            inp.outer_input.short_name = self.name[:3]
            inp.outer_input.save()
        if self.type=='output':
            inp = self.inputs.all()[0]
            inp.short_name = self.name[:3]
            inp.name = self.name
            inp.save()
            inp.outer_output.name = self.name
            inp.outer_output.short_name = self.name[:3]
            inp.outer_output.save()
        try:
            w_link = self.workflow_link
            w_link.name=new_name
            w_link.save()
        except Workflow.DoesNotExist:
            pass

    def run(self,offline):
        """ This is only a hack, to make this work on windows """
        try: 
            if self.abstract_widget.windows_queue:
                t = runWidget.apply_async([self,offline],queue="windows")
                t.wait()
            else:
                self.proper_run(offline)
        except AttributeError:
            self.proper_run(offline)

    def proper_run(self,offline):
        """ This is the real start. """
        print("proper_run_widget")
        if not self.ready_to_run():
            raise WidgetException("The prerequisites for running this widget have not been met.")
        self.running=True
        self.save()
        if self.type == 'regular' or self.type == 'subprocess':
            """ if this is a subprocess or a regular widget than true."""
            if not self.abstract_widget is None:
                """if this is an abstract widget than true we save the widget function in a variable """
                function_to_call = getattr(workflows.library,self.abstract_widget.action)
            input_dict = {}
            outputs = {}
            for i in self.inputs.all():
                """ we walk through all the inputs """
                #gremo pogledat ce obstaja povezava in ce obstaja gremo value prebrat iz outputa
                if not i.parameter:
                    """ if there is a connection than true and read the output value """
                    if i.connections.count() > 0:
                        i.value = i.connections.all()[0].output.value
                        i.save()
                    else:
                        i.value = None
                        i.save()
                if i.multi_id == 0:
                    input_dict[i.variable]=i.value
                else:
                    if not i.variable in input_dict:
                        input_dict[i.variable]=[]
                    if not i.value==None:
                        input_dict[i.variable].append(i.value)
            start = time.time()
            try:
                if not self.abstract_widget is None:
                    """ again, if this objects is an abstract widget than true and check certain parameters,
                    else check if is_for_loop"""
                    if self.abstract_widget.wsdl != '':
                        """ if abstrac widget is a web service """
                        input_dict['wsdl']=self.abstract_widget.wsdl
                        input_dict['wsdl_method']=self.abstract_widget.wsdl_method
                    if self.abstract_widget.has_progress_bar:
                        """ if abstrac widget has a progress bar """
                        outputs = function_to_call(input_dict,self)
                    elif self.abstract_widget.is_streaming:
                        """ if abstrac widget is a stream """
                        outputs = function_to_call(input_dict,self,None)
                    else:
                        """ else run abstract widget function """
                        outputs = function_to_call(input_dict)
                else:
                    if self.workflow_link.is_for_loop():
                        """ if this is object is a for loop than true and run;
                        else false and run workflow """
                        print("proper_run_is_for_loop")
                        self.workflow_link.run_for_loop()
                        #print self.outputs.all()[0].value
                    elif self.workflow_link.is_cross_validation():
                        self.workflow_link.run_cross_validation()
                    else:
                        self.workflow_link.run()
            except:
                self.error=True
                self.running=False
                self.finished=False
                self.save()
                raise
            elapsed = (time.time()-start)
            outputs['clowdflows_elapsed']=elapsed
            for o in self.outputs.all():
                """ we walk through all the outputs """
                if not self.abstract_widget is None:
                    """ if this object is an abstract widget than true and save output
                    else look for outputs in workflow """
                    try:
                        o.value = outputs[o.variable]
                    except:
                        pass
                    o.save()
                else:
                    #gremo v outpute pogledat
                    if not self.workflow_link.is_for_loop():
                        o.value = o.inner_input.value
                        o.save()
            if self.abstract_widget is None:
                """ if object is widget than true and configure parameters """
                self.finished=True
                self.running=False
                self.error=False
                self.save()
            else:
                if not self.abstract_widget.interactive or offline:
                    self.finished=True
                    self.running=False
                    self.error=False
                    self.save()
            cons = Connection.objects.filter(output__widget=self)
            for c in cons:
                c.input.widget.unfinish()
            return outputs
        elif self.type == 'for_input':
            """ if object is an input widget for for loop than read all input values and finish """
            #print("for_input")
            for o in self.outputs.all():
                o.value=o.outer_input.value
                #print(o.outer_input.value)
                o.save()
            self.finished=True
            self.running=False
            self.error=False
            self.save()
        elif self.type == 'for_output':
            """ if object is an output widget for for loop, then read output values and 
            configure parameters"""
            #print("for_output")
            for i in self.inputs.all():
                if not i.parameter:
                    """ if there is a connection than true and read the output value """
                    if i.connections.count() > 0:
                        i.value = i.connections.all()[0].output.value
                        i.save()
                        i.outer_output.value.append(i.value)
                        i.outer_output.save()
                        self.finished=True
            self.finished=True
            self.running=False
            self.error=False
            self.save()
        elif self.type == 'cv_input':
            """ if object is an input widget for cross validation 
            than read all input values and finish """
            for o in self.outputs.all():
                #print('cv_input')
                o.value=o.outer_input.value
                o.save()
            self.finished=True
            self.running=False
            self.error=False
            self.save()
        elif self.type == 'cv_output':
            """ if object is an output widget for cross validation, 
            then read output values and configure parameters"""
            for i in self.inputs.all():
                if not i.parameter:
                    """ if there is a connection than true and read the output value """
                    if i.connections.count() > 0:
                        if i.value is None:
                            i.value = [i.connections.all()[0].output.value]
                        else:
                            i.value = [i.connections.all()[0].output.value] + i.value
                        #print i.value
                        i.save()
                        i.outer_output.value.append(i.value)
                        i.outer_output.save()
                        self.finished=True
            self.finished=True
            self.running=False
            self.error=False
            self.save()
        elif self.type == 'input':
            """ if object is an input widget for for loop than read all input values and finish """
            for o in self.outputs.all():
                o.value=o.outer_input.value
                o.save()
            self.finished=True
            self.running=False
            self.error=False
            self.save()
        elif self.type == 'output':
            """ if object is an output widget, then read output values and 
            configure parameters"""
            for i in self.inputs.all():
                if not i.parameter:
                    """ if there is a connection than true and read the output value """
                    if i.connections.count() > 0:
                        i.value = i.connections.all()[0].output.value
                        i.save()
                        i.outer_output.value = i.value
                        i.outer_output.save()
                        self.finished=True
            self.finished=True
            self.running=False
            self.error=False
        return None

    def reset(self,offline):
        #for i in self.inputs.all():
        #    if not i.parameter:
        #        i.value = None
        #        i.save()
        #for i in self.outputs.all():
        #    i.value = None
        #    i.save()
        self.finished = False
        self.error = False
        self.running = False
        self.save()
        if self.type == 'subprocess':
            self.subunfinish()

    def reset_descendants(self):
        """ Method resets all the widget connections/descendants. """
        pairs = []
        for c in self.workflow.connections.select_related("output","input").defer("output__value","input__value").all():
            if not (c.output.widget_id,c.input.widget_id) in pairs:
                pairs.append((c.output.widget_id,c.input.widget_id))
        next = {}                
        for p in pairs:
            if not next.has_key(p[0]):
                next[p[0]]=set()
            next[p[0]].add(p[1])
        widgets_that_need_reset = set([self.pk,])
        current_widgets_that_need_reset = set([self.pk,])
        while len(current_widgets_that_need_reset)>0:
            new_widgets_that_need_reset = set()
            for w_id in current_widgets_that_need_reset:
                try:
                    for p in next.get(w_id):
                        new_widgets_that_need_reset.add(p)
                        widgets_that_need_reset.add(p)
                except:
                    pass
            current_widgets_that_need_reset = new_widgets_that_need_reset
        Widget.objects.filter(id__in=widgets_that_need_reset).update(finished=False,error=False,running=False)
        subprocesses = Widget.objects.filter(id__in=widgets_that_need_reset,type='subprocess')
        for w in subprocesses:
            w.subunfinish()
        return widgets_that_need_reset

    def reset_descendants_slow(self):
        #find all descendants and reset them as well
        widgets = list(self.workflow.widgets.prefetch_related('inputs','outputs','inputs__connections','outputs__connections','outputs__connections__input','inputs__connections__output'))
        widgets_dict = {}
        widgets_that_need_reset = set([self.pk,])
        current_widgets_that_need_reset = set([self.pk,])
        for w in widgets:
            widgets_dict[w.pk]=w
        while len(current_widgets_that_need_reset)>0:
            new_widgets_that_need_reset = set()
            for w_id in current_widgets_that_need_reset:
                for o in widgets_dict[w_id].outputs.all():
                    for c in o.connections.all():
                        new_widgets_that_need_reset.add(c.input.widget_id)
                        widgets_that_need_reset.add(c.input.widget_id)
            current_widgets_that_need_reset = new_widgets_that_need_reset
        Widget.objects.filter(id__in=widgets_that_need_reset).update(finished=False,error=False,running=False)
        for w in widgets_that_need_reset:
            if widgets_dict[w].type == 'subprocess':
                widgets_dict[w].subunfinish()
        #    widgets_dict[w].reset(False)
        return widgets_that_need_reset

    def run_post(self,request):
        if not self.ready_to_run():
            raise WidgetException("The prerequisites for running this widget have not been met.")
        self.running=True
        self.save()
        function_to_call = getattr(workflows.library,self.abstract_widget.post_interact_action)
        input_dict = {}
        outputs = {}
        output_dict = {}
        for o in self.outputs.all():
            output_dict[o.variable]=o.value
        for i in self.inputs.all():
            #gremo pogledat ce obstaja povezava in ce obstaja gremo value prebrat iz outputa
            if not i.parameter:
                if i.connections.count() > 0:
                    i.value = i.connections.all()[0].output.value
                    i.save()
                else:
                    i.value = None
                    i.save()
            if i.multi_id == 0:
                input_dict[i.variable]=i.value
            else:
                if not i.variable in input_dict:
                    input_dict[i.variable]=[]
                if not i.value==None:
                    input_dict[i.variable].append(i.value)
        try:
            if not self.abstract_widget is None:
                if self.abstract_widget.windows_queue:
                    t = executeWidgetPostInteract.apply_async([self,input_dict,output_dict,request],queue="windows")
                    outputs = t.wait()
                else:
                    outputs = executeWidgetPostInteract(self,input_dict,output_dict,request)
            else:
                self.workflow_link.run()
        except:
            self.error=True
            self.running=False
            self.finished=False
            self.save()
            raise
        for o in self.outputs.all():
            o.value = outputs[o.variable]
            o.save()
        self.finished=True
        self.running=False
        self.error=False
        self.interaction_waiting=False
        self.save()
        cons = Connection.objects.filter(output__widget=self)
        for c in cons:
            c.input.widget.unfinish()
        return outputs

    def __unicode__(self):
        return unicode(self.name)

class Input(models.Model):
    name = models.CharField(max_length=200)
    short_name = models.CharField(max_length=3)
    description = models.TextField(blank=True,null=True)
    variable = models.CharField(max_length=50)
    widget = models.ForeignKey(Widget,related_name="inputs")
    required = models.BooleanField()
    parameter = models.BooleanField()
    value = PickledObjectField(null=True)
    multi_id = models.IntegerField(default=0)
    inner_output = models.ForeignKey('Output',related_name="outer_input_rel",blank=True,null=True) #za subprocess
    outer_output = models.ForeignKey('Output',related_name="inner_input_rel",blank=True,null=True) #za subprocess
    PARAMETER_CHOICES = (
        ('text','Single line'),
        ('textarea','Multi line text'),
        ('select', 'Select box'),
    )
    parameter_type = models.CharField(max_length=50,choices=PARAMETER_CHOICES,blank=True,null=True)
    order = models.PositiveIntegerField(default=1)

    class Meta:
        ordering = ('order',)

    def __unicode__(self):
        return unicode(self.name)

"""class InputCrossValidation(models.Model):
    name = models.CharField(max_length=200)
    short_name = models.CharField(max_length=3)
    description = models.TextField(blank=True,null=True)
    variable = models.CharField(max_length=50)
    widget = models.ForeignKey(Widget,related_name="inputs2")
    required = models.BooleanField()
    parameter = models.BooleanField()
    value = PickledObjectField(null=True)
    multi_id = models.IntegerField(default=0)
    inner_output1 = models.ForeignKey('OutputCrossValidation',related_name="outer_input_rel",blank=True,null=True) #za subprocess
    #inner_output2 = models.ForeignKey('OutputCrossValidation',related_name="outer_input_rel",blank=True,null=True) #za subprocess
    outer_output = models.ForeignKey('OutputCrossValidation',related_name="inner_input_rel",blank=True,null=True) #za subprocess
    PARAMETER_CHOICES = (
        ('text','Single line'),
        ('textarea','Multi line text'),
        ('select', 'Select box'),
    )
    parameter_type = models.CharField(max_length=50,choices=PARAMETER_CHOICES,blank=True,null=True)
    order = models.PositiveIntegerField(default=1)

    class Meta:
        ordering = ('order',)

    def __unicode__(self):
        return unicode(self.name)

class OutputCrossValidation(models.Model):
    name = models.CharField(max_length=200)
    short_name = models.CharField(max_length=5)
    description = models.TextField(blank=True)
    variable = models.CharField(max_length=50)
    widget = models.ForeignKey(Widget,related_name="outputs")
    value = PickledObjectField(null=True)
    inner_input = models.ForeignKey(InputCrossValidation,related_name="outer_output_rel",blank=True,null=True) #za subprocess
    outer_input = models.ForeignKey(InputCrossValidation,related_name="inner_output_rel",blank=True,null=True) #za subprocess
    order = models.PositiveIntegerField(default=1)

    class Meta:
        ordering = ('order',)

    def __unicode__(self):
        return unicode(self.name)"""

class Option(models.Model):
    input = models.ForeignKey(Input,related_name="options")
    name = models.CharField(max_length=200)
    value = models.TextField(blank=True,null=True)

    class Meta:
        ordering = ['name']

class Output(models.Model):
    name = models.CharField(max_length=200)
    short_name = models.CharField(max_length=5)
    description = models.TextField(blank=True)
    variable = models.CharField(max_length=50)
    widget = models.ForeignKey(Widget,related_name="outputs")
    value = PickledObjectField(null=True)
    inner_input = models.ForeignKey(Input,related_name="outer_output_rel",blank=True,null=True) #za subprocess
    outer_input = models.ForeignKey(Input,related_name="inner_output_rel",blank=True,null=True) #za subprocess
    order = models.PositiveIntegerField(default=1)

    class Meta:
        ordering = ('order',)

    def __unicode__(self):
        return unicode(self.name)

class UserProfile(models.Model):
    user = models.OneToOneField(User,related_name="userprofile")
    active_workflow = models.ForeignKey(Workflow,related_name="users",null=True,blank=True,on_delete=models.SET_NULL)

    def __unicode__(self):
        return unicode(self.user)

def create_user_profile(sender, instance, created, **kwargs):
    profile_set = UserProfile.objects.filter(user__id = instance.id)
    if created and not profile_set.exists():
        UserProfile.objects.create(user=instance)

# nardi da k nardimo userja da se avtomatsko nardi se UserProfile
post_save.connect(create_user_profile, sender=User)

def copy_workflow(old, user, parent_widget_conversion={},parent_input_conversion={},parent_output_conversion={},parent_widget=None):
    w = Workflow()
    if parent_widget is None:
        w.name = old.name+" (copy)"
    else:
        w.name = old.name
    w.user = user
    w.public = False
    w.description = old.description
    w.template_parent = old
    if not parent_widget is None:
        w.widget = parent_widget
    w.save()
    widget_conversion = {}
    input_conversion = {}
    output_conversion = {}
    for widget in old.widgets.all():
        new_widget = Widget()
        new_widget.workflow = w
        new_widget.x = widget.x
        new_widget.y = widget.y
        new_widget.name = widget.name
        new_widget.abstract_widget = widget.abstract_widget
        new_widget.finished = widget.finished
        new_widget.error = widget.error
        new_widget.running = widget.running
        new_widget.interaction_waiting = widget.interaction_waiting
        new_widget.type = widget.type
        new_widget.progress = widget.progress
        new_widget.save()
        widget_conversion[widget.id]=new_widget.id
        for input in widget.inputs.all():
            new_input = Input()
            new_input.name = input.name
            new_input.short_name = input.short_name
            new_input.description = input.description
            new_input.variable = input.variable
            new_input.widget = new_widget
            new_input.required = input.required
            new_input.parameter = input.parameter
            new_input.value = input.value
            new_input.multi_id = input.multi_id
            #inner_output nikol ne nastavlamo
            #outer_output in njemu spremenimo inner input
            if not parent_widget is None:
                if not input.outer_output is None:
                    new_input.outer_output = Output.objects.get(pk=parent_output_conversion[input.outer_output.id])
            new_input.parameter_type = input.parameter_type
            new_input.save()
            for option in input.options.all():
                new_option = Option()
                new_option.input = new_input
                new_option.name = option.name
                new_option.value = option.value
                new_option.save()
            if not parent_widget is None:
                if not input.outer_output is None:
                    new_input.outer_output.inner_input = new_input
                    new_input.outer_output.save()
            input_conversion[input.id]=new_input.id
        for output in widget.outputs.all():
            new_output = Output()
            new_output.name = output.name
            new_output.short_name = output.short_name
            new_output.description = output.description
            new_output.variable = output.variable
            new_output.widget = new_widget
            new_output.value = output.value
            #inner input nikol ne nastavlamo
            #outer input in njemu spremenimo inner output
            if not parent_widget is None:
                if not output.outer_input is None:
                    new_output.outer_input = Input.objects.get(pk=parent_input_conversion[output.outer_input.id])
            new_output.save()
            if not parent_widget is None:
                if not output.outer_input is None:
                    new_output.outer_input.inner_output = new_output
                    new_output.outer_input.save()
            output_conversion[output.id]=new_output.id
    for connection in old.connections.all():
        new_connection = Connection()
        new_connection.workflow = w
        new_connection.output = Output.objects.get(pk=output_conversion[connection.output.id])
        new_connection.input = Input.objects.get(pk=input_conversion[connection.input.id])
        new_connection.save()
    for widget in old.widgets.filter(type='subprocess'):
        #tuki mormo vse subprocesse zrihtat
        copy_workflow(widget.workflow_link, user, widget_conversion, input_conversion, output_conversion, Widget.objects.get(pk=widget_conversion[widget.id]))
    return w
