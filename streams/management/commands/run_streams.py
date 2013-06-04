from django.core.management.base import NoArgsCommand
from streams.models import *

class Command(NoArgsCommand):
    help = 'check for streams that need to be executed and execute them'
    option_list = NoArgsCommand.option_list
    def handle_noargs(self, **options):
        streams = Stream.objects.filter(active=True)
        for stream in streams:
            #preverimo ce je ze dost casa pretekl
            import django
            now = django.utils.timezone.now()
            delta = now - stream.last_executed
            delta_seconds = delta.seconds + delta.days * 86400
            if delta_seconds > stream.period or 1==1:
                stream.last_executed = now
                stream.save()
                print stream.execute()

