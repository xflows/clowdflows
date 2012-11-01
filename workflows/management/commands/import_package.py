from django.core.management.base import BaseCommand, CommandError
from workflows.models import Category, AbstractWidget, AbstractInput, AbstractOutput, AbstractOption
from django.core import serializers
from collections import Counter
from optparse import make_option


class Command(BaseCommand):
    args = 'file_name'
    help = 'Imports all models from the file named "file_name". All models in the database which have the same uuid as imported models are updated. The folowing models are included in inport: AbstractWidget, Category, AbstractInput, AbstractOutput, and AbstractOption.'
    option_list = BaseCommand.option_list + (
        make_option('-r', '--replace',
            action="store_true",
            dest='replace',
            default=False,
            help='Completely replace whole widgets with the new one where UIDs match. Default behaviour merges widgets submodels (AbstractInputs, AbstractOutputs and AbstratcOptions)'
                 'based on their submodel\'s own UID. When using this option all widget\'s old submodels are deleted and completely replaced by new submodels.)'
        ),
    )

    def handle(self, *args, **options):
        if (len(args)<1):
            raise CommandError('Arguments "file_name" and "package_name" are required!')

        try:
            instr = open(args[0], 'r').read()
        except:
            raise CommandError('There was a problem with opening given input file')

        self.importString(self, instr, options['replace'])
        self.stdout.write('Import procedure successfully finished.\n')

    def importString(self, obj, instr, replace):
        #get all objects from file and eliminate empty UID and check for UID duplicates
        objsFile = list(serializers.deserialize("json", instr))
        objsFileNoUid = [x for x in objsFile if len(x.object.uid) == 0]
        objsFile = [x for x in objsFile if len(x.object.uid) != 0]
        if len(objsFileNoUid)>0:
            obj.stdout.write('File contains %i model(s) without UID field set. Those will not be imported! If you wish to'
                              ' assign them random UIDs then use the "-n" option when exporting models with the "export_package"'
                              ' command. Afterwards, you will be able to import them.\n' % len(objsFileNoUid))
        if len(Counter([x.object.uid for x in objsFile])) != len(objsFile):
            raise CommandError('Input process terminated without any changes to the database. There were multiple equal '
                               'UIDs defined on different models in the given input file. The input procedure can not continue '
                               'from safety reasons. Please resolve manually!')

        #divide new objects by type
        wids = [x for x in objsFile if isinstance(x.object, AbstractWidget)]
        inps = [x for x in objsFile if isinstance(x.object, AbstractInput)]
        outs = [x for x in objsFile if isinstance(x.object, AbstractOutput)]
        opts = [x for x in objsFile if isinstance(x.object, AbstractOption)]
        cats = [x for x in objsFile if isinstance(x.object, Category)]

        #ouput statistics about file
        obj.stdout.write('Import contains:\n')
        obj.stdout.write('    % 4i AbstractWidget(s)\n' % len(wids))
        obj.stdout.write('    % 4i AbstractInput(s)\n' % len(inps))
        obj.stdout.write('    % 4i AbstractOutput(s)\n' % len(outs))
        obj.stdout.write('    % 4i AbstractOption(s)\n' % len(opts))
        obj.stdout.write('    % 4i Category(s)\n' % len(cats))

        #get all objects from database
        objsDb = []
        objsDb.extend(AbstractWidget.objects.all())
        objsDb.extend(AbstractInput.objects.all())
        objsDb.extend(AbstractOutput.objects.all())
        objsDb.extend(AbstractOption.objects.all())
        objsDb.extend(Category.objects.all())

        #check for DB UID duplicates
        objsdbDict = dict((x.uid,x) for x in objsDb if len(x.uid) != 0)
        if len([x for x in objsDb if len(x.uid) != 0]) != len(objsdbDict):
            raise CommandError('Input process terminated without any changes to the database. There were multiple equal '
                               'UIDs defined on different models in the database. The input procedure can not continue '
                               'from safety reasons. Please resolve manually!')

        #create new to existing id mapping and check for type match
        idMappingDict = dict()
        for objFile in objsFile:
            if objsdbDict.has_key(objFile.object.uid):
                objDb = objsdbDict[objFile.object.uid]
                objFileTypeId = str(type(objFile.object))+':'+str(objFile.object.id)
                objDbTypeId = str(type(objDb))+':'+str(objDb.id)
                if type(objFile.object) == type(objsdbDict[objFile.object.uid]):
                    idMappingDict[objFileTypeId] = objDb.id
                else:
                    raise CommandError('Input process terminated without any changes to the database. Two models match by uid but not '
                                       'by type:\n    - from file: %s\n    - from database: %s\n    Please resolve manually!'% (objFileTypeId, objDbTypeId))

        #ouput statistics about database
        obj.stdout.write('Current database contains %i models,\n' % len(objsDb))
        obj.stdout.write('    of which %i models have UID set,\n' % len(objsdbDict))
        obj.stdout.write('    of which %i models match with the imported models and will be updated.\n' % len(idMappingDict))

        #prepare statistics
        statDict = dict([('old:'+str(t),len(t.objects.all())) for t in [AbstractWidget, AbstractInput, AbstractOutput, AbstractOption, Category]])
        for objFile in objsFile:
            for operation in ['mod','add','del']:
                statDict[operation+':'+str(type(objFile.object))]=0

        #sort so that AbstractWidgets come in front (are processsed first in the following block)
        #objsFileNew = []
        #objsFileNew.extend([objFile for objFile in objsFile if isinstance(objFile.object, AbstractWidget)])
        #objsFileNew.extend([objFile for objFile in objsFile if not isinstance(objFile.object, AbstractWidget)])
        #objsFile = objsFileNew

        #save models to the database - update the ids for the matching models and remove the ids (to get a new one) for the non matching models
        obj.stdout.write('Merging file and database models ...')
        importedUids = dict()
        for objFile in objsFile:
            objFileTypeId = str(type(objFile.object))+':'+str(objFile.object.id)
            if isinstance(objFile.object, AbstractWidget):
                objFile.old_category_id = objFile.object.category_id
            if isinstance(objFile.object, AbstractInput):
                objFile.old_widget_id = objFile.object.widget_id
            if isinstance(objFile.object, AbstractOutput):
                objFile.old_widget_id = objFile.object.widget_id
            if isinstance(objFile.object, AbstractOption):
                objFile.old_abstract_input_id = objFile.object.abstract_input_id
            if isinstance(objFile.object, Category):
                if not objFile.object.parent_id is None:
                    objFile.old_parent_id = objFile.object.parent_id

            if idMappingDict.has_key(objFileTypeId):
                #there is already an existing model with same uid
                statDict['mod:'+str(type(objFile.object))]+=1
                objFile.object.id = idMappingDict[objFileTypeId]
            else:
                #there is no model jet, add it
                statDict['add:'+str(type(objFile.object))]+=1
                objFile.object.id = None
            objFile.save()
            idMappingDict[objFileTypeId] = objFile.object.id
            importedUids[objFile.object.uid]=True
        obj.stdout.write(' done.\n')

        #correct also the foreign keys
        obj.stdout.write('Updating model\'s foreign keys ...')
        for objFile in wids:
            objFile.object.category = Category.objects.get(id=idMappingDict[str(Category)+':'+str(objFile.old_category_id)])
            objFile.save()
        for objFile in inps:
            objFile.object.widget = AbstractWidget.objects.get(id=idMappingDict[str(AbstractWidget)+':'+str(objFile.old_widget_id)])
            objFile.save()
        for objFile in outs:
            objFile.object.widget = AbstractWidget.objects.get(id=idMappingDict[str(AbstractWidget)+':'+str(objFile.old_widget_id)])
            objFile.save()
        for objFile in opts:
            objFile.object.abstract_input = AbstractInput.objects.get(id=idMappingDict[str(AbstractInput)+':'+str(objFile.old_abstract_input_id)])
            objFile.save()
        for objFile in cats:
            if not objFile.object.parent_id is None:
                objFile.object.parent = Category.objects.get(id=idMappingDict[str(Category)+':'+str(objFile.old_parent_id)])
                objFile.save()
        obj.stdout.write(' done.\n')

        if replace:
            obj.stdout.write('Removing unnecessary inputs/options/outputs...')
            for wid in [wid for wid in objsFile if isinstance(wid.object, AbstractWidget)]:
                for inp in AbstractInput.objects.filter(widget = wid.object.id):
                    for opt in AbstractOption.objects.filter(abstract_input = inp.id):
                        if not importedUids.has_key(opt.uid):
                            statDict['del:'+str(AbstractOption)]+=1
                            opt.delete()
                    if not importedUids.has_key(inp.uid):
                        statDict['del:'+str(AbstractInput)]+=1
                        inp.delete()
                for out in AbstractOutput.objects.filter(widget = wid.object.id):
                    if not importedUids.has_key(out.uid):
                        statDict['del:'+str(AbstractOutput)]+=1
                        out.delete()
            obj.stdout.write(' done.\n')

        #update and output statistics
        statDict = dict(statDict.items() + dict([('new:'+str(t),len(t.objects.all())) for t in [AbstractWidget, AbstractInput, AbstractOutput, AbstractOption, Category]]).items())
        obj.stdout.write('Database models count statistics: pre-import + ( added | modified | deleted ) = after-import\n')
        for t in [AbstractWidget, AbstractInput, AbstractOutput, AbstractOption, Category]:
            obj.stdout.write('    % 15s: % 5i + (% 4i | % 4i | % 4i ) = % 5i\n' %
                             (t.__name__,
                              statDict['old:'+str(t)],
                              statDict['add:'+str(t)],
                              statDict['mod:'+str(t)],
                              statDict['del:'+str(t)],
                              statDict['new:'+str(t)]))
