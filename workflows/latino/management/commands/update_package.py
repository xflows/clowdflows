from django.core.management.base import BaseCommand, CommandError
from django.core import serializers
from collections import Counter
from optparse import make_option
from workflows.latino.import_dotnet import LatinoCF
from workflows.latino.import_dotnet import ClowdFlowsDefinitions
from pprint import pprint
import sys
from workflows.latino.settings import create_backups
from workflows.latino.settings import package_root
import workflows.management.commands.import_package
import os
import shutil
from datetime import datetime
from workflows.latino import serialization_utils

class Command(BaseCommand):
    args = 'file_name'
    help = 'Export all widgets marked in the c# dll package as widgets and directly imports them to the mothra database.' \
           'As if "latino.generate_package a.json" followed by "workflows.import_package a.json" would be called.' \
           '!However!, the export file goes to the standard place (latino/db/package_data.json) and replaces the existing file.' \
           'Variable "create_backups" in "latino/setings.py" controls whether backups of package_data.json are kept in "latino/backups".'
    option_list = BaseCommand.option_list + (
        make_option('-f', '--force',
            action="store_true",
            dest='force',
            default=False,
            help='By default, the old "package_data.json" and newly generated definition strings are compared. In case there are no differences'
                 'nothing happens. This switch forces (regardles of equality) the import of the newly generated data and creation of backups '
                 '(if latino/setings.py/create_backups == True).'
            ),
        make_option('-r', '--replace',
            action="store_true",
            dest='replace',
            default=False,
            help='Completely replace whole widgets with the new one where UIDs match. Default behaviour merges widgets sub-models (AbstractInputs, AbstractOutputs and AbstratcOptions)'
                 'based on their sub-model\'s own UID. When using the option "--replace" then all widget\'s old submodels are deleted and completely replaced by new sub-models.)'
            ),
        )

    def handle(self, *args, **options):
        timeStamp = datetime.now().strftime('.backup_%Y%m%d_%H%M%S.json')
        backupDir = os.path.join(package_root,"backup")
        dataFileName = "package_data.json"
        dataFilePath = os.path.join(package_root,"db",dataFileName)
        dataFileBackupPath = os.path.join(backupDir,dataFileName)+timeStamp
        codeFileName = "library_gen.py"
        codeFilePath = os.path.join(package_root, codeFileName)
        codeFileBackupPath = os.path.join(backupDir,codeFileName)+timeStamp

        self.stdout.write('Creating json and python code representations of widgets, inputs, outputs, options and categories ... ')
        newDataCode = serialization_utils.ToPyObj(ClowdFlowsDefinitions.Get())
        newDataDef = newDataCode['json']
        newCodeDef = newDataCode['library']
        self.stdout.write('done.\n')

        self.stdout.write('Reading previous json definitions if existing and compare to the new ... ')
        oldDataDef = ""
        try:
            oldDataDef = open(dataFilePath).read()
        except:
            pass
        self.stdout.write('done.\n')

        dataReplace = True
        if oldDataDef == newDataDef:
            self.stdout.write('The newly generated data is the same as the file "latino/db/package_data.json".\n')
            if not options['force']:
                self.stdout.write('Nothing will be done with the data definition (use "--force" option to force the import).\n')
                dataReplace = False
        else:
            self.stdout.write('The newly generated data is new or different from the file "latino/db/package_data.json".\n')


        self.stdout.write('Reading previous code definitions if existing and compare to the new ... ')
        oldCodeDef = ""
        try:
            oldCodeDef = open(codeFilePath).read()
        except:
            pass
        self.stdout.write('done.\n')

        codeReplace = True
        if oldCodeDef == newCodeDef:
            self.stdout.write('The newly generated code is the same as the file "latino/library_gen.py".\n')
            if not options['force']:
                self.stdout.write('Nothing will be done with the code file (use "--force" option to force the code replace).\n')
                dataReplace = False
        else:
            self.stdout.write('The newly generated definitions are new or different from the file "latino/library_gen.py".\n')

        if (dataReplace or codeReplace) and create_backups:
            if not os.path.exists(backupDir):
                os.makedirs(backupDir)

        if dataReplace and create_backups and os.path.exists(dataFilePath):
            try:
                shutil.move(dataFilePath, dataFileBackupPath)
                self.stdout.write('Backup of database successfully created.\n')
            except:
                raise CommandError('There was a problem with the operation of backing up the data file. '
                                   'Procedure update_package was terminated without any changes to the database.')

        if dataReplace:
            open(dataFilePath,"w").write(newDataDef)
            self.stdout.write('New data file successfully written to its location.\n')
            importString = workflows.management.commands.import_package.Command()
            importString.import_package_string(self, newDataDef, options['replace'])
            self.stdout.write('Widget definitions in the database were updated successfully from the dll library.\n')

        if codeReplace and create_backups and os.path.exists(codeFilePath):
            try:
                shutil.move(codeFilePath, codeFileBackupPath)
                self.stdout.write('Backup of code successfully created.\n')
            except:
                raise CommandError('There was a problem with the operation of backing up the code file. Procedure update_package terminated.')

        if codeReplace:
            open(codeFilePath,"w").write(newCodeDef)
            self.stdout.write('New code file successfully written to its location.\n')

        self.stdout.write('Procedure update_package successfully finished.\n')


