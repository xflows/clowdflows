from django.core.management.base import BaseCommand, CommandError
from workflows.models import Category, AbstractWidget, AbstractInput, AbstractOutput, AbstractOption
from django.core import serializers
from optparse import make_option
import uuid

class Command(BaseCommand):
    args = 'file_name package_name <package_name2, package_name3, ...>'
    help = 'Exports all AbstractWidgets from a package "package_name" to a file named "file_name". Exports also all needed connected models Category, AbstractInput, AbstractOutput, and AbstractOption.'
    option_list = BaseCommand.option_list + (
        make_option('-n', '--newuid',
            action="store_true",
            dest='newuid',
            default=False,
            help='UID field of all exported models will be overwritten with new random values.'),
        make_option('-a', '--all',
            action="store_true",
            dest='all',
            default=False,
            help='Export all widgets regardless of the specified package'),
        )

    def handle(self, *args, **options):
        if (len(args)<2):
            raise CommandError('Arguments "file_name" and "package_name" are required!')

        try:
            f = open(args[0], 'w')
        except:
            raise CommandError('There was a problem with creating/overwriting given output file')

        objs = []
        argsIter = iter(args)
        next(argsIter)
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
        if options['newuid']:
            for a in objs:
                a.uid = str(uuid.uuid4())

        outstr = ""
        if len(objs)>0:
            outstr = serializers.serialize("json", objs, indent=2, ensure_ascii=False)

        try:
            f.write(outstr)
        except:
            raise CommandError('There was a problem with writing to the given output file')

        self.stdout.write('Export procedure successfully finished. Results written to the file.\n')