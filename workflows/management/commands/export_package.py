from unicodedata import category
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

        result = self.export_package_string(self.stdout.write, args[1:], options['newuid'], options['all'], int(options['verbosity']))

        try:
            f.write(result.encode('utf-8'))
        except:
            raise CommandError('There was a problem with writing to the given output file')

        self.stdout.write('Export procedure successfully finished. Results written to the file.\n')

    def print_stataistics(self, objs, verbosity, write):
        if verbosity > 0:
            write('Selection contains:\n')
            write('    % 4i AbstractWidget(s)\n' % len([obj for obj in objs if isinstance(obj, AbstractWidget)]))
            write('    % 4i AbstractInput(s)\n' % len([obj for obj in objs if isinstance(obj, AbstractInput)]))
            write('    % 4i AbstractOutput(s)\n' % len([obj for obj in objs if isinstance(obj, AbstractOutput)]))
            write('    % 4i AbstractOption(s)\n' % len([obj for obj in objs if isinstance(obj, AbstractOption)]))
            write('    % 4i Category(s)\n' % len([obj for obj in objs if isinstance(obj, Category)]))

            if (verbosity == 1):
                write('Exported categories:\n')
            if (verbosity == 2):
                write('Exported categories and widgets:\n')
            if (verbosity == 2):
                write('Exported categories, widgets, inputs, outputs and options:\n')
            indent = 0
            indentAddon = 0
            for obj in objs:
                s = ''
                if isinstance(obj, Category):
                    indent = str(obj).count('::')
                    s = '% 3i. Category ===== %s =====' % (obj.order, obj)
                if isinstance(obj, AbstractWidget):
                    s = '    % 3i. AbstractWidget: %s [%s]' % (obj.order, obj.name, obj.action)
                if isinstance(obj, AbstractInput):
                    s = '        % 3i. AbstractInput: (%s) %s' % (obj.order, obj.short_name, obj.name)
                if isinstance(obj, AbstractOutput):
                    s = '        % 3i. AbstractOutput: (%s) %s' % (obj.order, obj.short_name, obj.name)
                if isinstance(obj, AbstractOption):
                    s = '                AbstractOption: %s | %s' % (obj.name, obj.value)

                if isinstance(obj, Category) or (isinstance(obj, AbstractWidget) and verbosity > 1) or verbosity > 2:
                    write('    ' * indent + s + '\n')

            self.stdout.write(
                'Tip: use higher "verbosity" option numbers to se more detailed output of what is being exported.\n')

    def export_package_string(self, write, packages, newuid, all, verbosity):
        assert isinstance(packages, tuple)
        assert isinstance(newuid, bool)
        assert isinstance(all, bool)

        objs = []
        for topCat in Category.objects.filter(parent = None):
            objs.extend(self.get_package_objs_in_category(topCat, packages, all))

        if len(objs) == 0:
            write('Selected package(s) were not found!\n')
            return "[\\n]"

        #be careful uid is only changed on these instances and is not written to the database
        if newuid:
            for a in objs:
                a.uid = str(uuid.uuid4())

        self.print_stataistics(objs, verbosity, write)

        result = serializers.serialize("json", objs, indent=2, ensure_ascii=False)

        return result

    def get_package_objs_in_category(self, cat, packages, all):
        assert isinstance(cat, Category)
        assert isinstance(packages, tuple)
        assert isinstance(all, bool)

        objs = []

        objs.extend(self.get_package_wids_in_category(cat, packages, all))

        for catChild in cat.children.all():
            objs.extend(self.get_package_objs_in_category(catChild, packages, all))

        if len(objs)>0:
            objs.insert(0,cat)

        return objs

    def get_package_wids_in_category(self, cat, packages, all):
        assert isinstance(cat, Category)
        assert isinstance(packages, tuple)
        assert isinstance(all, bool)

        objs = []

        if all:
            wids = cat.widgets.all()
        else:
            wids = cat.widgets.filter(package__in=packages)

        for wid in wids:
            objs.append(wid)
            for inp in wid.inputs.all():
                objs.append(inp)
                objs.extend(inp.options.all())
            objs.extend(wid.outputs.all())

        return objs
