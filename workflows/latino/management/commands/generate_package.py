from django.core.management.base import BaseCommand, CommandError
from workflows.models import Category, AbstractWidget, AbstractInput, AbstractOutput, AbstractOption
from django.core import serializers
from collections import Counter
from optparse import make_option
from workflows.latino.latino import LatinoCF
from pprint import pprint
import sys

class Command(BaseCommand):
    args = 'file_name'
    help = 'Export all widgets marked in the c# dll package as widgets to the json file which can be imported to the mothra database'

    def handle(self, *args, **options):
        if (len(args)<1):
            raise CommandError('Argument "file_name" is required!')

        try:
            f = open(args[0], 'w')
        except:
            raise CommandError('There was a problem with creating/overwriting given output file')

        self.stdout.write('Creating json representations of widgets, inputs, outputs, options and categories ... ')
        outstr = LatinoCF.GetJsonDbDefinitions()
        self.stdout.write('done.\n')

        try:
            f.write(outstr)
        except:
            raise CommandError('There was a problem with writing to the given output file')

        self.stdout.write('Json definitions successfully created. Results written to the file.\n')