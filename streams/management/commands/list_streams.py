from django.core.management.base import NoArgsCommand
from streams.models import *

class Command(NoArgsCommand):
    help = 'see a list of all streams'
    option_list = NoArgsCommand.option_list
    def handle_noargs(self, **options):
        import time
        self.stdout.write("Working on streams...")
        self.stdout.flush()
        while True:
            streams = Stream.objects.filter(active=True)
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
                    self.stdout.write(u"<"+str(now)+">Executing "+unicode(stream)+"...")
                    self.stdout.flush()
                    stream.execute()
                    self.stdout.write("done!\n")
                    self.stdout.flush()
                    #print stream.execute()
            time.sleep(1)
            #self.stdout.write(".")
            #self.stdout.flush()

