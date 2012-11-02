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
        if (len(args) < 2):
            raise CommandError('Arguments "file_name" and "package_name" are required!')

        try:
            f = open(args[0], 'w')
        except:
            raise CommandError('There was a problem with creating/overwriting given output file')

        result = self.export_package_string(self.stdout.write, args[1:], options['newuid'], options['all'])

        try:
            f.write(result)
        except:
            raise CommandError('There was a problem with writing to the given output file')

        self.stdout.write('Export procedure successfully finished. Results written to the file.\n')

    def export_package_string(self, write, packages, newuid, all):
        result = []

        objs = []
        if all:
            packages = ['all']
        for package in packages:
            if all:
                wids = AbstractWidget.objects.all()
            else:
                wids = AbstractWidget.objects.filter(package=package)
            inps = AbstractInput.objects.filter(widget__in=[x.id for x in wids])
            outs = AbstractOutput.objects.filter(widget__in=[x.id for x in wids])
            opts = AbstractOption.objects.filter(abstract_input__in=[x.id for x in inps])
            cats = Category.objects.filter(id__in=[x.category.id for x in wids])

            #retrieve all parents
            catNum = len(cats)
            while True:
                cats = cats | Category.objects.filter(id__in=[x.parent.id for x in cats if x.parent != None])
                if catNum == len(cats):
                    break
                else:
                    catNum = len(cats)

            objs.extend(cats)
            objs.extend(wids)
            objs.extend(outs)
            objs.extend(inps)
            objs.extend(opts)

            if len(wids) > 0:
                write('Package "%s" contains:\n' % package)
                write('    % 4i AbstractWidget(s)\n' % len(wids))
                write('    % 4i AbstractInput(s)\n' % len(inps))
                write('    % 4i AbstractOutput(s)\n' % len(outs))
                write('    % 4i AbstractOption(s)\n' % len(opts))
                write('    % 4i Category(s)\n' % len(cats))
            else:
                write('Package "%s" was not found!\n' % package)

        #be careful uid is only changed on these instances and is not written to the database
        if newuid:
            for a in objs:
                a.uid = str(uuid.uuid4())

        result = ""
        if len(objs) > 0:
            result = serializers.serialize("json", objs, indent=2, ensure_ascii=False)

        return result