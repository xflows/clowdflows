from django.core.management.base import BaseCommand, CommandError
from workflows.models import Category, AbstractWidget, AbstractInput, AbstractOutput, AbstractOption
from django.core import serializers

#dirty python2.6 fix
try:
    from collections import Counter
except:
    def Counter(list):
        return set(list)

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
            raise CommandError('Arguments "file_name" is required!')

        try:
            string = open(args[0], 'r').read()
        except:
            raise CommandError('There was a problem with opening given input file')

        import_package_string(self.stdout.write, string, options['replace'], int(options['verbosity']))
        self.stdout.write('Import procedure successfully finished.\n')


def import_package_string(writeFunc, string, replace, verbosity=1):
    #get all objects from file and eliminate empty UID and check for UID duplicates
    objsFileRaw = serializers.deserialize("json", string)
    objsFile = list(objsFileRaw)

    #order file models - essential for succesfull import
    #TODO: following ordering could be done more efficiently
    objsFile = order_objects_hier_top(objsFile)

    objsFileNoUid = [x for x in objsFile if len(x.object.uid) == 0]
    objsFile = [x for x in objsFile if len(x.object.uid) != 0]
    if len(objsFileNoUid)>0:
        writeFunc('File contains %i model(s) without UID field set. Those will not be imported! If you wish to'
                  ' assign them random UIDs then use the "-n" option when exporting models with the "export_package"'
                  ' command. Afterwards, you will be able to import them.\n' % len(objsFileNoUid))
    if len(Counter([x.object.uid for x in objsFile])) != len(objsFile):
        a = sorted([x.object.uid for x in objsFile])
        for x in a:
            print x
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
    if verbosity>0:
        writeFunc('Import file contains:\n')
        writeFunc('    % 4i AbstractWidget(s)\n' % len(wids))
        writeFunc('    % 4i AbstractInput(s)\n' % len(inps))
        writeFunc('    % 4i AbstractOutput(s)\n' % len(outs))
        writeFunc('    % 4i AbstractOption(s)\n' % len(opts))
        writeFunc('    % 4i Category(s)\n' % len(cats))

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
        error_txt=  'Input process terminated without any changes to the database. There were multiple equal ' \
                    'UIDs defined on different models in the database. The input procedure can not continue ' \
                    'from safety reasons. Please resolve manually! UIDs with multiple models:'
        #count objects per uid
        from collections import defaultdict
        objs_per_uid=defaultdict(list)
        for x in objsDb:
            if x.uid:
                objs_per_uid[x.uid].append(x)

        for uid,objs in objs_per_uid.items():
            if len(objs)>1:
                error_txt+="\n\nUID:     "+str(uid)+"\nobjects: "+str(objs)
        raise CommandError(error_txt)

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
                                   'by type:\n    - from file: id: %s uid: %s\n    - from database: id: %s uid: %s\n    Please resolve manually!'%
                                   (objFileTypeId, objFile.object.uid, objDbTypeId, objsdbDict[objFile.object.uid].uid))

    #ouput statistics about database
    if verbosity>0:
        writeFunc('Current database contains %i models,\n' % len(objsDb))
        writeFunc('    of which %i models have UID set,\n' % len(objsdbDict))
        writeFunc('    of which %i models match with the imported models and will be updated.\n' % len(idMappingDict))

    #prepare statistics
    statDict = dict([('old:'+str(t),len(t.objects.all())) for t in [AbstractWidget, AbstractInput, AbstractOutput, AbstractOption, Category]])
    for modelType in [AbstractWidget, AbstractInput, AbstractOutput, AbstractOption, Category]:
        for operation in ['mod','add','del']:
            statDict[operation+':'+str(modelType)]=0

    #save models to the database - update the ids for the matching models and remove the ids (to get a new one) for the non matching models
    #the import needs to be done in specific order! Hierarhically top down - all superiror objects needs to be imported prior importing sub object
    #order: parent categories>sub categories>widgets>inputs>outputs>options

    if verbosity>0:
        writeFunc('Merging file and database models ...' + ('\n' if verbosity>1 else ''))
    importedUids = dict()
    for objFile in objsFile:
        objFileTypeId = str(type(objFile.object))+':'+str(objFile.object.id)

        if verbosity>1:
            objFileTypeIdStr = objFileTypeId.replace(":",":"+" "*(47-len(objFileTypeId)))
            if idMappingDict.has_key(objFileTypeId):
                writeFunc('updating: ' + objFileTypeIdStr + ' => <db_id>: ' + str(idMappingDict[objFileTypeId]) + '\n')
            else:
                writeFunc('  adding: ' + objFileTypeIdStr + '\n')

        #parent category needs to be already imported and added to idMappingDict
        if isinstance(objFile.object, Category):
            if not objFile.object.parent_id is None:
                objId = idMappingDict[str(Category)+':'+str(objFile.object.parent_id)]
                if verbosity>2:
                    writeFunc('% 52s'%'rewiring parent category from <file_id>:' + '% 5i'%objFile.object.parent_id + ' => <db_id>: %i\n'%objId)
                objFile.object.parent = Category.objects.get(id=objId)

        #widget's category needs to be already imported and added to idMappingDict
        if isinstance(objFile.object, AbstractWidget):
            objId = idMappingDict[str(Category) + ':' + str(objFile.object.category_id)]
            if verbosity>2:
                writeFunc('% 52s'%'rewiring widget\'s category from <file_id>:' + '% 5i'%objFile.object.category_id + ' => <db_id>: %i\n'%objId)
            objFile.object.category = Category.objects.get(id=objId)

        #input/output's widget needs to be already imported and added to idMappingDict
        if isinstance(objFile.object, AbstractInput) or isinstance(objFile.object, AbstractOutput):
            objId = idMappingDict[str(AbstractWidget) + ':' + str(objFile.object.widget_id)]
            if verbosity>2:
                writeFunc('% 52s'%'rewiring containing widget from <file_id>:' + '% 5i'%objFile.object.widget_id + ' => <db_id>: %i\n'%objId)
            objFile.object.widget = AbstractWidget.objects.get(id=objId)

        #options's input needs to be already imported and added to idMappingDict
        if isinstance(objFile.object, AbstractOption):
            objId = idMappingDict[str(AbstractInput) + ':' + str(objFile.object.abstract_input_id)]
            if verbosity>2:
                writeFunc('% 52s'%'rewiring containing input from <file_id>:' + '% 5i'%objFile.object.abstract_input_id + ' => <db_id>: %i\n'%objId)
            objFile.object.abstract_input = AbstractInput.objects.get(id=objId)

        #update existing model or add a new one
        if idMappingDict.has_key(objFileTypeId):
            #there is already an existing model with same uid
            statDict['mod:'+str(type(objFile.object))]+=1
            objFile.object.id = idMappingDict[objFileTypeId]
        else:
            #there is no model jet, add it
            statDict['add:'+str(type(objFile.object))]+=1
            objFile.object.id = None

        objFile.save() #actual saving to the DB, if object is new then id is assigend at this point

        #dictionary bookkeeping
        idMappingDict[objFileTypeId] = objFile.object.id
        importedUids[objFile.object.uid]=True

    if verbosity>0:
        writeFunc(' done.\n')

    if replace:
        if verbosity>0:
            writeFunc('Removing unnecessary inputs/options/outputs...')
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
        if verbosity>0:
            writeFunc(' done.\n')

    #update and output statistics
    if verbosity>0:
        statDict = dict(statDict.items() + dict([('new:'+str(t),len(t.objects.all())) for t in [AbstractWidget, AbstractInput, AbstractOutput, AbstractOption, Category]]).items())
        writeFunc('Database models count statistics: pre-import + ( added | modified | deleted ) = after-import\n')
        for t in [AbstractWidget, AbstractInput, AbstractOutput, AbstractOption, Category]:
            writeFunc('    % 15s: % 5i + (% 4i | % 4i | % 4i ) = % 5i\n' %
                      (t.__name__,
                       statDict['old:'+str(t)],
                       statDict['add:'+str(t)],
                       statDict['mod:'+str(t)],
                       statDict['del:'+str(t)],
                       statDict['new:'+str(t)]))

def order_objects_hier_top(objsFile):
    objsFileOrdered = []
    for topCat in [x for x in objsFile if (isinstance(x.object, Category) and x.object.parent_id is None)]:
        objsFileOrdered.extend(order_objects_hier(topCat, objsFile))
    return objsFileOrdered

def order_objects_hier(cat, objsFile):
    assert isinstance(cat.object, Category)
    assert isinstance(objsFile, list)
    objsFileOrdered = []

    objsFileOrdered.append(cat)
    for wid in [x for x in objsFile if (isinstance(x.object, AbstractWidget) and x.object.category_id == cat.object.id)]:
        objsFileOrdered.append(wid)
        for inp in [x for x in objsFile if (isinstance(x.object, AbstractInput) and x.object.widget_id == wid.object.id)]:
            objsFileOrdered.append(inp)
            for opt in [x for x in objsFile if (isinstance(x.object, AbstractOption) and x.object.abstract_input_id == inp.object.id)]:
                objsFileOrdered.append(opt)
        for outp in [x for x in objsFile if (isinstance(x.object, AbstractOutput) and x.object.widget_id == wid.object.id)]:
            objsFileOrdered.append(outp)

    for subCat in [x for x in objsFile if (isinstance(x.object, Category) and x.object.parent_id == cat.object.id)]:
        objsFileOrdered.extend(order_objects_hier(subCat,objsFile))

    return objsFileOrdered