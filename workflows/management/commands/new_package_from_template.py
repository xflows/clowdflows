from django.core.management.base import BaseCommand, CommandError
from optparse import make_option
import uuid
from distutils import dir_util
import os
import sys
from mothra.settings import PROJECT_DIR

class Command(BaseCommand):
    args = 'new_package_name'
    help = 'Creates new package based on package_template. It generates new uids for template database objects, renames functions, renames files and registers package in INSTALLED_APPS.'

    def handle(self, *args, **options):
        if (len(args) < 1):
            raise CommandError('Argument "new_package_name" is required')
        self.stdout.write('Starting package initiation from template.\n')
        if (int(options['verbosity'])<2):
            self.stdout.write('Tip: use higher verbosity option number to see what is going on in detail.\n')
        new_package_from_template(self.stdout.write, args[0], int(options['verbosity']))
        self.stdout.write('Creating new package successfully finished. You might want to use import_package command now.\n')


def replace_in_files(files, topPath, fromStr, toStr, verbosity, writeFunc):
    if verbosity > 1:
        writeFunc("Replacing '%s' with '%s' in:\n" % (fromStr, toStr))
    for f in files:
        content = open(f, 'r').read()
        contentNew = content.replace(fromStr, toStr)
        if content != contentNew:
            open(f, 'w').write(contentNew)
            if verbosity > 2:
                writeFunc("    .%s\n" % (f[len(topPath):],))


def new_package_from_template(writeFunc, package, verbosity):
    pckTmpName = "package_template"
    pckTmpTitle = "Package Template"
    pckTmpPrefix = "pcktmp_"
    pckTmpUid = "uid_to_replace"
    pckPrefix = package + "_"
    subappPlaceholder = "    #WORKFLOWS_SUBAPP_PLACEHOLDER"
    subapp = "    \'workflows.%s\',\n"%package
    settFile = os.path.join(PROJECT_DIR,'settings.py')

    wfDir = os.path.abspath(os.path.join(PROJECT_DIR,"..","workflows"))
    templateDir = os.path.join(wfDir,pckTmpName)
    packageDir = os.path.join(wfDir, package)
    if os.path.exists(packageDir):
        raise Exception('Directory "%s" for package of given name already exists! Terminating procedure of creating new package.'%packageDir)
    copied = dir_util.copy_tree(templateDir, packageDir)

    if verbosity>1:
        writeFunc("Creating objects:\n")
        for f in copied:
            if verbosity>2:
                writeFunc("    .%s\n"%f[len(wfDir):])

    replace_in_files(copied, wfDir, pckTmpPrefix, pckPrefix, verbosity, writeFunc)
    replace_in_files(copied, wfDir, pckTmpTitle, package[:1].upper()+package[1:], verbosity, writeFunc)
    replace_in_files(copied, wfDir, pckTmpName, package, verbosity, writeFunc)

    if verbosity>1:
        writeFunc("Replacing temporary uids in:\n")
    for f in copied:
        content = open(f, 'r').read()
        contentNew = content
        while contentNew.count(pckTmpUid)>0:
            contentNew = contentNew.replace(pckTmpUid, str(uuid.uuid4()), 1)
        if content != contentNew:
            open(f, 'w').write(contentNew)
            if verbosity>2:
                writeFunc("    .%s\n"%(f[len(wfDir):],))

    if verbosity>1:
        writeFunc("Renaming files:\n")
    for f in copied:
        if f[len(wfDir):].count(pckTmpPrefix)>0:
            fNew = f.replace(pckTmpPrefix, pckPrefix)
            os.rename(f,fNew)
            if verbosity>2:
                writeFunc("    .%s => .%s\n"%(f[len(wfDir):],fNew[len(wfDir):]))

    if verbosity>1:
        writeFunc("Renaming directories:\n")
    for f in [x[0] for x in os.walk(packageDir)]:
        if f.endswith(pckTmpName):
            fNew = f.replace(pckTmpName, package)
            os.rename(f,fNew)
            if verbosity>2:
                writeFunc("    .%s => .%s\n"%(f[len(wfDir):],fNew[len(wfDir):]))

    #if verbosity>1:
    #    writeFunc("Adding package to INSTALLED_APPS variable in .%s\n"%settFile[len(PROJECT_DIR):])
    #content = open(settFile, 'r').read()
    #place = content.find(subappPlaceholder)
    #contentNew = content[:place] + subapp + content[place:]
    #open(settFile, 'w').write(contentNew)