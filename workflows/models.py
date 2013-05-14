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
    name = models.CharField(max_length=200,default='Untitled workflow')
    user = models.ForeignKey(User,related_name="workflows")
    public = models.BooleanField(default=False)
    description = models.TextField(blank=True,default='')
    widget = models.OneToOneField('Widget',related_name="workflow_link",blank=True,null=True)
    template_parent = models.ForeignKey('Workflow',blank=True,null=True,default=None,on_delete=models.SET_NULL)
    
    
    def is_for_loop(self):
        if self.widgets.filter(type='for_input').count()>0:
            return True
        else:
            return False    
    
    def get_ready_to_run(self):
        widgets = self.widgets.all()
        unfinished_list = []
        for w in widgets:
            if not w.finished and not w.running:
                ready_to_run = True
                connections = self.connections.filter(input__widget=w)
                for c in connections:
                    if not c.output.widget.finished:
                        ready_to_run = False
                        break
                if ready_to_run:
                    unfinished_list.append(w.id)
        return unfinished_list
        
    def get_runnable_widgets(self):
        widgets = self.widgets.all()
        unfinished_list = []
        for w in widgets:
            if not w.finished and not w.running:
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
        #clear for_input and for_output
        fi = self.widgets.filter(type='for_input')[0]
        fo = self.widgets.filter(type='for_output')[0]
        outer_output = fo.inputs.all()[0].outer_output
        outer_output.value=[]
        outer_output.save()
        
        input_list = fi.outputs.all()[0].outer_input.value
        progress_total = len(input_list)
        current_iteration = 0
        for i in input_list:
            fi.unfinish()
            fo.unfinish()
            proper_output = fi.outputs.all()[0]
            proper_output.value = i
            proper_output.save()
            fi.finished=True
            fi.save()
            if not USE_CONCURRENCY or 1==1:
                unfinished_list = self.get_runnable_widgets()
                try:
                    while len(unfinished_list)>0:
                        for w in unfinished_list:
                            w.run(True)
                            total = self.widgets.count()
                            completed = self.widgets.filter(finished=True).count()    
                            self.widget.progress = (int)((current_iteration*100.0/progress_total)+(((completed*1.0)/total)*(100/progress_total)))
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
    wsdl = models.URLField(verify_exists=False,max_length=200,blank=True,help_text='WSDL and WSDL method are used if the widget is a call of a Web Service. Web Service widgets are usually not entered in the admin panel, but in the application itself by importing a Web Service.')
    wsdl_method = models.CharField(max_length=200,blank=True,default='')
    description = models.TextField(blank=True,help_text='Description is used for a human readable description of what a widget does. A user will see this when he right clicks the widget and clicks help.')
    category = models.ForeignKey(Category,related_name="widgets",help_text='Category determines to which category this widget belongs. Categories can be nested.')
    visualization_view = models.CharField(max_length=200,blank=True,default='',help_text='Visualization view is (like the action) a python function that is a view that will render a template.')
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
    workflow = models.ForeignKey(Workflow,related_name="widgets")
    x = models.IntegerField()
    y = models.IntegerField()
    name = models.CharField(max_length=200)
    abstract_widget = models.ForeignKey(AbstractWidget,related_name="instances",blank=True,null=True)
    finished = models.BooleanField(default=False)
    error = models.BooleanField(default=False)
    running = models.BooleanField(default=False)
    interaction_waiting = models.BooleanField(default=False)
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
        if self.finished or self.error:
            self.finished=False
            self.error=False
            self.progress=0
            self.save()
            cons = Connection.objects.filter(output__widget=self)
            for c in cons:
                if c.input.widget.finished:
                    c.input.widget.unfinish()
        if self.type == 'subprocess':
            for w in self.workflow_link.widgets.all():
                w.finished=False
                w.error=False
                w.save()
                if w.type=='subprocess':
                    w.subunfinish()
                    
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
        if not self.ready_to_run():
            raise Exception("The prerequisites for running this widget have not been met.")
        self.running=True
        self.save()
        if self.type == 'regular' or self.type == 'subprocess':
            if not self.abstract_widget is None:
                function_to_call = getattr(workflows.library,self.abstract_widget.action)
            input_dict = {}
            outputs = {}
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
                    if self.abstract_widget.wsdl != '':
                        input_dict['wsdl']=self.abstract_widget.wsdl
                        input_dict['wsdl_method']=self.abstract_widget.wsdl_method
                    if self.abstract_widget.has_progress_bar:
                        outputs = function_to_call(input_dict,self)
                    elif self.abstract_widget.is_streaming:
                        outputs = function_to_call(input_dict,self,None)
                    else:
                        outputs = function_to_call(input_dict)
                else:
                    if self.workflow_link.is_for_loop():
                        self.workflow_link.run_for_loop()
                        #print self.outputs.all()[0].value
                    else:
                        self.workflow_link.run()
            except:
                self.error=True
                self.running=False
                self.finished=False
                self.save()
                raise
            for o in self.outputs.all():
                if not self.abstract_widget is None:
                    o.value = outputs[o.variable]
                    o.save()
                else:
                    #gremo v outpute pogledat
                    if not self.workflow_link.is_for_loop():
                        o.value = o.inner_input.value
                        o.save()
            if self.abstract_widget is None:
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
            for o in self.outputs.all():
                o.value=o.outer_input.value
                o.save()
            self.finished=True
            self.running=False
            self.error=False
            self.save()
        elif self.type == 'for_output':
            for i in self.inputs.all():
                if not i.parameter:
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
        elif self.type == 'input':
            for o in self.outputs.all():
                o.value=o.outer_input.value
                o.save()
            self.finished=True
            self.running=False
            self.error=False
            self.save()
        elif self.type == 'output':
            for i in self.inputs.all():
                if not i.parameter:
                    if i.connections.count() > 0:
                        i.value = i.connections.all()[0].output.value
                        i.save()
                        i.outer_output.value = i.value
                        i.outer_output.save()
                        self.finished=True
            self.finished=True                        
            self.running=False
            self.error=False
            self.save()
        return None
        
    def reset(self,offline):
        for i in self.inputs.defer("value").all():
            if not i.parameter:
                i.value = None
                i.save()
        for i in self.outputs.defer("value").all():
            i.value = None
            i.save()
        self.finished = False
        self.error = False
        self.running = False
        self.save()

    def run_post(self,request):
        if not self.ready_to_run():
            raise Exception("The prerequisites for running this widget have not been met.")
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
                outputs = function_to_call(request,input_dict, output_dict)
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