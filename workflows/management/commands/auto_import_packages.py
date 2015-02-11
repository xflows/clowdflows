from datetime import datetime
import os
import sys
from django.core.management.base import BaseCommand, CommandError
from workflows import module_importer
from workflows.management.commands import export_package_old as export_package
from workflows.management.commands import import_package_old as import_package
from optparse import make_option

class Command(BaseCommand):
    help = 'Automatically iterates through all installed workflows sub-applications/projects/packages and imports their database entires. ' \
           'Note: Installed workflows packages are defined in mothra/settings.py via variable INSTALLED_APPS and begin with the string "workflows.". ' \
           'Auto import procedure does the following:\n' \
           '    - Creates database export of all definition objects using export_package command.\n'\
           '    - Export file goes to folder specified in mothra/settings.py/BACKUP_DIR and is timestamped\n'\
           '    For each installed package:\n' \
           '    - Loads package settings from "workflows/<package_name>/settings.py\n' \
           '    - If settings do not exist or settings.py/AUTO_IMPORT_DB == False then exit\n' \
           '    - Else tries to import all the files specified in settings.py/AUTO_IMPORT_DB_FILES list\n' \
           '    - If some files are missing skip them.\n' \
           '    - Imports are done using import_package command using -r option if settings.py/AUTO_IMPORT_DB_REPLACE_OPTION == True'

    option_list = BaseCommand.option_list + (
        make_option('-n', '--nobackup',
            action="store_true",
            dest='nobackup',
            default=False,
            help='No backup is created prior starting the import process.'
        ),
        make_option('-a', '--ask',
                    action="store_true",
                    dest='ask',
                    default=False,
                    help='Ask to import packages which are marked not to be imported.'
        ),
    )

    def handle(self, *args, **options):
        auto_import_all_packages(self.stdout.write, options['nobackup'], options['ask'])
        self.stdout.write('Auto import procedure finished.\n')

def auto_import_all_packages(writeFunc, nobackup, ask):
    if ask:
        writeFunc('The procedure will interactively ask to import packages marked as not to be auto imported due to "--ask" option.\n')
    if nobackup:
        writeFunc('No backup will be created due to "--nobackup" option.\n')
    else:
        try:
            from mothra.settings import BACKUP_DIR
        except:
            raise CommandError('Do not know where to backup existing database: BACKUP_DIR variable not found in mothra/settings.py. Consider using "--nobackup" option.')
        if not os.path.exists(BACKUP_DIR): os.makedirs(BACKUP_DIR)
        timeStamp = datetime.now().strftime('_%Y%m%d_%H%M%S.json')
        backupDir = os.path.join(BACKUP_DIR,"db_backup"+timeStamp)

        writeFunc('Exporting to backup...\n')
        result = export_package.export_package_string(lambda text: writeFunc('    '+text), ('all',), False, False, True, 1)
        try:
            f = open(backupDir, 'w')
            f.write(result.encode('utf-8'))
            f.close()
            writeFunc('Backup successfully written.\n')
        except Exception as e:
            raise CommandError('There was a problem with writing to the given backup file "%s". Problem: %s'%(backupDir, e))
        writeFunc('Export procedure successfully finished. Results written to the file "%s".\n' %backupDir)

    #get all relevant package settings:
    packageSetts = module_importer.import_all_packages_libs_as_dict("settings")
    for pckSett in packageSetts:
        writeFunc('--------------------------------------------------------------------------------\n')
        writeFunc('Auto importing package "%s":\n'%pckSett)

        sett = packageSetts[pckSett]
        if sett is None:
            writeFunc('    No settings found for this package.\n')
            continue

        try:
            imp = sett.AUTO_IMPORT_DB
            files = sett.AUTO_IMPORT_DB_FILES
        except:
            writeFunc('    Either AUTO_IMPORT_DB or AUTO_IMPORT_DB_FILES not found in package\'s settings.\n')
            continue

        replace = False
        try:
            replace = sett.AUTO_IMPORT_DB_REPLACE_OPTION
        except:
            pass

        if not imp:
            writeFunc('    AUTO_IMPORT_DB set to false in package\'s settings.\n')
            if not ask or not query_yes_no('    Do you want to import this package anyway?\n'):
                continue

        for fileName in files:
            writeFunc('   Importing file "%s":\n' % fileName)
            try:
                fileContent = open(fileName, 'r').read()
            except:
                writeFunc('        Cannot open or read given package data file.\n')
            else:
                import_package.import_package_string(lambda text: writeFunc('        '+text), fileContent, replace)
            writeFunc('   Done with file "%s":\n' % fileName)

    writeFunc('--------------------------------------------------------------------------------\n')
    return

def query_yes_no(question, default=None):
    """Ask a yes/no question via raw_input() and return their answer.

    "question" is a string that is presented to the user.
    "default" is the presumed answer if the user just hits <Enter>.
        It must be "yes" (the default), "no" or None (meaning
        an answer is required of the user).

    The "answer" return value is one of "yes" or "no".
    """
    valid = {"yes":True,   "y":True,  "ye":True,
             "no":False,     "n":False}
    if default == None:
        prompt = " [y/n] "
    elif default == "yes":
        prompt = " [Y/n] "
    elif default == "no":
        prompt = " [y/N] "
    else:
        raise ValueError("invalid default answer: '%s'" % default)

    while True:
        sys.stdout.write(question + prompt)
        choice = raw_input().lower()
        if default is not None and choice == '':
            return valid[default]
        elif choice in valid:
            return valid[choice]
        else:
            sys.stdout.write("Please respond with 'yes' or 'no' " \
                             "(or 'y' or 'n').\n")