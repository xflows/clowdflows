from django.core.management.base import BaseCommand, CommandError
from workflows.models import Category, AbstractWidget, AbstractInput, AbstractOutput, AbstractOption
from django.core import serializers
from optparse import make_option
import uuid

class Command(BaseCommand):
    args = 'package_name <package_name2, package_name3, ...>'
    help = 'DANGEROUS OPERATION which renews UID fields of all AbstractWidgets, Categories, AbstractInputs, AbstractOutputs, and AbstractOptions from the given package!'
    option_list = BaseCommand.option_list + (
        make_option('--YES_SAVE_TO_DB',
            action="store_true",
            dest='renew',
            default=False,
            help='Since the operation of UIDs renewal is dangerous, it must be executed with this option in order to modify DB. '
                 'This is just a precaution to make sure that the person executing it, understands the risk.'),
        make_option('-a', '--all',
            action="store_true",
            dest='all',
            default=False,
            help='Export all widgets regardless of the specified package'),
        )

    def handle(self, *args, **options):
        if (len(args)<1):
            raise CommandError('At least one "package_name" is required!')


        objs = []
        argsIter = iter(args)
        if options['all']:
            argsIter = ['all']
        for package in argsIter:
            if options['all']:
                wids = AbstractWidget.objects.all()
            else:
                wids = AbstractWidget.objects.filter(package=package)
            inps = AbstractInput.objects.filter(widget__in = [x.id for x in wids])
            outs = AbstractOutput.objects.filter(widget__in = [x.id for x in wids])
            opts = AbstractOption.objects.filter(abstract_input__in = [x.id for x in inps])
            cats = Category.objects.filter(id__in = [x.category.id for x in wids])

            #retrieve all parents
            catNum = len(cats)
            while True:
                cats = cats | Category.objects.filter(id__in = [x.parent.id for x in cats if x.parent != None])
                if catNum == len(cats):
                    break
                else:
                    catNum = len(cats)

            objs.extend(cats)
            objs.extend(wids)
            objs.extend(outs)
            objs.extend(inps)
            objs.extend(opts)

            if len(wids)>0:
                self.stdout.write('Package "%s" contains:\n' % package)
                self.stdout.write('    % 4i AbstractWidget(s)\n' % len(wids))
                self.stdout.write('    % 4i AbstractInput(s)\n' % len(inps))
                self.stdout.write('    % 4i AbstractOutput(s)\n' % len(outs))
                self.stdout.write('    % 4i AbstractOption(s)\n' % len(opts))
                self.stdout.write('    % 4i Category(s)\n' % len(cats))
            else:
                self.stdout.write('Package "%s" was not found!\n' % package)

        #be careful uid is only changed on these instances and is not written to the database
        for a in objs:
            a.uid = str(uuid.uuid4())
            if options['renew']:
                a.save()

        self.stdout.write('UID renew procedure successfully finished. ')
        if options['renew']:
            self.stdout.write('Database was modified!\n')
        else:
            self.stdout.write('Database was NOT modified! See help of the command, especially the option YES_SAVE_TO_DB.\n')