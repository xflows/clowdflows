from django.core.management.base import NoArgsCommand
from workflows.models import *

import os

class Command(NoArgsCommand):
    help = 'This command fills the static image field of every abstract widget in the database that doesn\'t have the static image field set (by using the data from the media). Use with care.'
    option_list = NoArgsCommand.option_list
    def handle_noargs(self, **options):
        self.stdout.write('Working')
        self.stdout.flush()
        ctr = 0
        for a in AbstractWidget.objects.all():
            if a.image and not a.static_image:
                a.static_image = os.path.basename(a.image.name)
                a.save()
                self.stdout.write('.')
                self.stdout.flush()
                ctr = ctr + 1
        self.stdout.write(' done\n')
        self.stdout.write('%i widgets affected.\n' % ctr)
