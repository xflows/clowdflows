from django.core.management.base import NoArgsCommand
from streams.models import *
from workflows import library

class Command(NoArgsCommand):
    help = 'check for streams that need to be executed and execute them'
    option_list = NoArgsCommand.option_list
    def handle_noargs(self, **options):
        import time
        self.stdout.write("Working on streams...")
        self.stdout.flush()

        while True:
            streams = Stream.objects.filter(active=True).filter(workflow__widgets__abstract_widget__uid='74421454-58ad-4dad-bac9-e1a78d147be6')
            for stream in streams:
                #self.stdout.write(u"\nChecking stream "+unicode(stream)+"...\n")
                #self.stdout.flush()
                #preverimo ce je ze dost casa pretekl
                import django
                now = django.utils.timezone.now()
                delta = now - stream.last_executed
                delta_seconds = delta.seconds + delta.days * 86400
                if delta_seconds > stream.period:
                    stream.last_executed = now
                    stream.save()
                    self.stdout.write(u"Executing "+unicode(stream)+"...")
                    self.stdout.flush()
                    
                    widgets = stream.workflow.widgets.filter(abstract_widget__is_streaming=True)
                    outputs = {}
                    for w in widgets:
                        input_dict = {}
                        output_dict = {}
                        finish = True
                        try:
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
                            
                            function_to_call = getattr(workflows.library,w.abstract_widget.action)
                            function_to_call(input_dict,w,stream)
                        except Exception as e:                         
                            print('Something went wrong: ', e)
                    self.stdout.write("done!\n")
                    self.stdout.flush()
            time.sleep(1)
            

