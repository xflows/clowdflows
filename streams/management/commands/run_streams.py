from django.core.management.base import NoArgsCommand
from streams.models import *

class Command(NoArgsCommand):
    help = 'check for streams that need to be executed and execute them'
    option_list = NoArgsCommand.option_list
    def handle_noargs(self, **options):
        print 'test'
