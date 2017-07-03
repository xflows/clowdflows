from django.core.management.base import NoArgsCommand
from streams.models import *

class Command(NoArgsCommand):
    help = 'check for streams that need to be executed and execute them'
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
                    self.stdout.write(u"Executing "+unicode(stream)+"...")
                    self.stdout.flush()
                    try:
                        stream.execute()
                    except:
                        import traceback
                        self.stdout.write("\n ERROR in executing stream:\n")
                        traceback.print_exc(file=self.stdout)
                    self.stdout.write("done!\n")
                    self.stdout.flush()
                    #print stream.execute()
            time.sleep(1)
            #self.stdout.write(".")
            #self.stdout.flush()

