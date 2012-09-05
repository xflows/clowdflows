import orange, orngTree, random

def insertNoise(input_dict):
    data = input_dict['data']    
    meta_noisy = orange.EnumVariable("noise", values=["no", "yes"])
    mid = orange.newmetaid()
    while mid in data.domain.getmetas().keys():
        mid = orange.newmetaid()
    data.domain.addmeta(mid, meta_noisy)
    data.addMetaAttribute("noise", "no")
    # Generate random indices for noise insertion
    percent = float(input_dict['noise_level'])/100
    try:
        rnds = int(input_dict['rnd_seed'])
    except:
        rnds = 0
    print "Random Seed:", rnds
    orange.setrandseed(rnds)
    noise_indices = random.sample(range(len(data)), int(round(percent*len(data))))
    #print "Amount of added noise:", percent*100, "percent (", len(noise_indices), "examples ):"
    #print "Random indices for added noise:", noise_indices
    className = data.domain.classVar.name
    #print "Class name:", className
    for index in noise_indices:
        data[index]["noise"] = "yes"
        temp = data[index][className]
##        if len(data.domain.classVar.values) > 2:
        # random value + check if it is diferent from the current one
        new_label = data.domain.classVar.randomvalue()
        while new_label == temp:
            new_label = data.domain.classVar.randomvalue()
        data[index][className] = new_label
##        else:
##            # switch the class value
##            data[index][className] = data.domain.classVar.nextvalue(data[index][className])
        #print "\t", temp, "changed to:", data[index].getclass(), "(", index, ")"
    #print "\n"
    noise_indices.sort()
    return {'noise_inds':noise_indices, 'noisy_data': data}

def addMetaID(data):
    meta_id = orange.FloatVariable("meta_id")
    mid = orange.newmetaid()
    while mid in data.domain.getmetas().keys():
        mid = orange.newmetaid()
    data.domain.addmeta(mid, meta_id)
    for i in range(len(data)):
        data[i][meta_id] = i

def cfdecide(input_dict, widget):
    from pysimplesoap.client import SoapFault
    somelearner = input_dict['learner']
    print somelearner
    # SWITCH TO PROCESSING WITH WEKA CLASSIFIERS    
    if type(somelearner) == unicode:
        from services.webservice import WebService 
        wsutil = WebService('http://vihar.ijs.si:8092/Utilities?wsdl', float(input_dict['timeout']))
        name = ""
        try:
            name = wsutil.client.print_model(model = somelearner)['model_as_string']            
            print wsutil.client.print_model(model = somelearner), name
        except SoapFault:
            # TODO something
            print "Soap fault: unicode string is not a Weka classification learner/model."
            return {}
        return cfweka(input_dict, widget, name)
    else:
        return cforange(input_dict, widget)

def cforange(input_dict, widget):
    from workflows.helpers import UnpicklableObject 
    somelearner = input_dict['learner']
    print "Before generate"
    learner = somelearner if not isinstance(somelearner,UnpicklableObject) else somelearner.generate()
    print "After generate"
    data = input_dict['data']
    print len(data)
    addMetaID(data)
    print 'Before for loop'
    k = int(input_dict['k_folds'])
    noisyIndices = []
    selection = orange.MakeRandomIndicesCV(data, folds=k)
    count_noisy = [0]*k
    print 'Before for loop'
    for test_fold in range(k):
        train_data = data.select(selection, test_fold, negate=1)
        test_data = data.select(selection, test_fold)
        #print "\t\t", "Learned on", len(train_data), "examples"
        #file.flush()
        print 'Before classifier construction'
        #print learner.hovername if learner.hovername != None else "ni hovernamea"
        classifier = learner(train_data)
        print 'After classifier construction'
        for example in test_data:
            exclassified = classifier(example)
            if exclassified != None and exclassified != example.getclass():
                # selection_filter[int(example[meta_id].value)] = 0
                noisyIndices.append(int(example["meta_id"].value))
                count_noisy[test_fold] += 1
        # END test_data
        widget.progress = int((test_fold+1)*1.0/k*100)
        widget.save()
    # END test_fold
    return {'inds': sorted(noisyIndices), 'name': learner.name} 
    ##    filtered_data = data.select(selection_filter, 1)
    ##    noisy_data = data.select(selection_filter, 0)
    ##    return [filtered_data, noisy_data]=======

def cfweka(input_dict, widget, name):
    from services.webservice import WebService 
    wseval = WebService('http://vihar.ijs.si:8092/Evaluation?wsdl', float(input_dict['timeout']))
    wsutil = WebService('http://vihar.ijs.si:8092/Utilities?wsdl', float(input_dict['timeout']))
    somelearner = input_dict['learner']
    print somelearner
    data = input_dict['data']
#    arffstr = toARFFstring(data).getvalue()
#    #print arffstr
#    wekaInstances = wsutil.client.arff_to_weka_instances(arff = arffstr, class_index = odt.domain.index(odt.domain.classVar))
#    #print wekaInstances
#    model = wseval.client.build_classifier(learner = somelearner, instances = wekaInstances['instances'])
#    #return {}
    
#    addMetaID(data)
    k = int(input_dict['k_folds'])
    noisyIndices = []
    selection = orange.MakeRandomIndicesCV(data, folds=k)
    count_noisy = [0]*k
    for test_fold in range(k):
        train_arffstr = toARFFstring(data.select(selection, test_fold, negate=1)).getvalue()
        train_data = wsutil.client.arff_to_weka_instances(arff = train_arffstr, class_index = data.domain.index(data.domain.classVar))['instances']
        
        test_inds = [i for i in range(len(selection)) if selection[i] == test_fold ]
        test_arffstr = toARFFstring(data.select(selection, test_fold)).getvalue()
        test_data = wsutil.client.arff_to_weka_instances(arff = test_arffstr, class_index = data.domain.index(data.domain.classVar))['instances']
        #print "\t\t", "Learned on", len(train_data), "examples"
        #file.flush()
        print "pred cl build"
        classifier = wseval.client.build_classifier(learner = somelearner, instances = train_data)['classifier']
        print "po cl build"
        eval_test_data = wseval.client.apply_classifier(classifier = classifier, instances = test_data)
        print "po eval"
        for i in range(len(eval_test_data)):
            #print "Test data length:", len(test_data), "Test inds length:", len(test_inds), "Eval Test data length:", len(eval_test_data)  
            print i, "v for zanki", eval_test_data[i]['classes'], data[test_inds[i]].getclass()
            if eval_test_data[i]['classes'] != unicode(data[test_inds[i]].getclass()):
                # selection_filter[int(example[meta_id].value)] = 0
                noisyIndices.append(test_inds[i])
                count_noisy[test_fold] += 1
        # END test_data
        widget.progress = int((test_fold+1)*1.0/k*100)
        widget.save()
    # END test_fold
    return {'inds': sorted(noisyIndices), 'name': getWekaName(name)} 

def saturation_type(input_dict, widget):
    dataset = input_dict['data']
    addMetaID(dataset)
    satur_type = input_dict['satur_type']
    widget.progress = 0
    widget.save()
    data_len = len(dataset)
    #k = data_len/2
    progress_steps = (3*data_len**2 + 2*data_len)/8 # provided max allowed iter steps (k) = data_len/2
    if satur_type == 'prune':
        if not dataset.hasMissingValues():
            return pruneSF(dataset, widget, 1, progress_steps)
        else:
            raise Exception("Pre-pruned saturation filtering requires data WITHOUT missing values!")
    else:
        return saturation(dataset, widget)
    
def cmplx(set):
    classifier = orngTree.TreeLearner(set, sameMajorityPruning=1, mForPruning=0)
    return orngTree.countNodes(classifier)

def findNoise(data):
    n = len(data)
    noisiest = []
    gE = cmplx(data)
    print "\t\t", "Classifier complexity:", gE, "nodes"
    #file.flush()
    min = gE
    for i in range(n):
        selection = [1]*n
        selection[i] = 0
        Ex = data.select(selection)
        if len(Ex)== 0:
            print "\t\t", "Saturation Filtering FAILED!"
            #file.flush()
            return [0, []]
        else:
            gEx = cmplx(Ex)
        if gEx < min:
            noisiest = [i]
            min = gEx
            print "\t\t", "(%s." % int(data[i]["meta_id"]),"example excluded) Subset complexity:", gEx, "nodes"#, "(%s)" % data[i]["noise"].value
            #file.flush()
            #print data[i]
        elif gEx != gE and gEx == min:
            noisiest.append(i)
            print "\t\t", "(%s." % int(data[i]["meta_id"]),"example excluded) Subset complexity:", gEx, "nodes"#, "(%s)" % data[i]["noise"].value
            #file.flush()
            #print data[i]
    if noisiest != []:
        return [0, noisiest]
    else:
        return [1, []]

def saturation(dataset, widget):#input_dict, comp):
    #dataset = input_dict['data']
    print "\t","Saturation Filtering:"
    #file.flush()
    noisyA = orange.ExampleTable(dataset.domain)
    data_len = len(dataset)
    k = data_len/2
    progress_steps = (3*data_len**2 + 2*data_len)/8 # provided max allowed iter steps (k) = data_len/2
    prog_sum = widget.progress
    workSet = orange.ExampleTable(dataset)
    while k != 0:
        n = len(workSet)
        satfilter = findNoise(workSet)
        if satfilter == [1,[]]:
            print "\t\t", satfilter
            widget.progress = 100
            widget.save()
            break
        else:
            noisyExmpls = satfilter[1]
            #print noisyExmpls
            selection = [0]*n
            choose = random.choice(noisyExmpls)
            print "\t\t", "Randomly choose one noisy example among:", len(noisyExmpls),\
            #      "(%s. is added noise: %s)" % (int(workSet[choose]["meta_id"]), workSet[choose]["noise"].value)
            #file.flush()
            selection[choose] = 1
            noisyA.extend(workSet.select(selection))
            workSet = workSet.select(selection, negate=1)
            
            prog_sum += n*1.0/progress_steps*100           
            widget.progress = int(prog_sum)
            widget.save()
            print "widget prog: ", widget.progress, "n: ", n, "progress_steps:", progress_steps, "prog_sum:", prog_sum
        k -= 1
    print "\t\t", "Found:", len(noisyA), "examples.\n"
    #file.flush()
    noisyIndices = []
    for ex in noisyA:
        noisyIndices.append(int(ex["meta_id"].value))
    #return [noisyA, workSet]
    #return [noisyIndices, workSet]
    return {"inds" : sorted(noisyIndices), "name" : "SF"}

def findPrunableNoisy(node, minExmplsInLeaf):
    toPrune = []
    print "in find, toPrune:", toPrune
    if isinstance(node, orange.TreeNode):
        #print "Bu!"
        if node and node.branchSelector:
            #print "Bu111!"
            for branch in node.branches:
                if branch == None:
                    continue
                else:
                    if len(branch.examples) > minExmplsInLeaf + 0.5:
                        bla = findPrunableNoisy(branch, minExmplsInLeaf)
                        toPrune.extend(bla)
                    else:
                        print "Zapisal za brisanje"
                        for ex in branch.examples:
                            toPrune.append(int(ex["meta_id"].value))
            return toPrune
        return []
    else:
        raise TypeError, "TreeNode expected"

def excludePruned(dataset, classifier, minExmplsInLeaf):
    print "in exclude"
    toPrune = findPrunableNoisy(classifier.tree, minExmplsInLeaf)
    uniqueItems(toPrune)
    print "\t\t", "Leaves with", minExmplsInLeaf, "or less examples will be pruned."
    print "\t\t", "IDs of examples excluded by pruning:", toPrune
    #file.flush()
    #noisyA = orange.ExampleTable(dataset.domain)
    n = len(dataset)
    selection = [0]*n
    for index in toPrune:
        selection[index] = 1
    #noisyA.extend(dataset.select(selection))
    workSet = dataset.select(selection, negate=1)
    #return [noisyA, dataset]
    return [toPrune, workSet]
    
def uniqueItems(list):
    list.sort()
    k = 0
    while k < len(list)-1:
        if list[k+1] == list[k]:
            del list[k+1]
        else:
            k += 1

def pruneSF(data, widget, minExmplsInLeaf, progress_steps):
    print "\t", "Pruning + Saturation Filter:"
    #file.flush()
    classifier = orngTree.TreeLearner(data, sameMajorityPruning=1, mForPruning=0, storeExamples=1)
    print "\t\t", "Classifier complexity:\t", orngTree.countNodes(classifier), "nodes."
    #file.flush()
##    [noisyA, dataset] = excludePruned(data, classifier, minExmplsInLeaf)
    [noisePruned, dataset] = excludePruned(data, classifier, minExmplsInLeaf)
    print "\t\t", len(noisePruned), "example(s) were excluded by pruning."
    #file.flush()
    classifier2 = orngTree.TreeLearner(dataset, sameMajorityPruning=1, mForPruning=0, storeExamples=1)
    print "\t\t", "Pruned Classifier complexity:", orngTree.countNodes(classifier2), "nodes. "
    #file.flush()
    # Saturation filtering
##    [noisy_data, filtered_data] = saturation(dataset, "tree")
    
    n = len(data)
    #widget.progress = int(len(noisePruned)*1.0/len(data)*100)
    widget.progress = int(sum([n-i for i in range(len(noisePruned))])*1.0/progress_steps*100)
    widget.save()
    print "progress:", widget.progress

    #[noiseSF, filtered_data] = saturation(dataset, widget)#, "tree")
    noiseSF = saturation(dataset, widget)#, "tree")
    #print "\t\t", "Size of filtered dataset:", len(filtered_data)
    print "\t\t", "Noisy examples (", len(noiseSF["inds"])+len(noisePruned),"(",len(noisePruned),"pruned,",\
          len(noiseSF["inds"]), "SF ))\n"#: (class, id)"
    #file.flush()
    #noisy_data.sort(meta_id)
    #noiseSF.sort()
    # Merge both obtained sets of noisy examples
    #noisyA.extend(noisy_data)
    noisePruned.extend(noiseSF["inds"])
    #return noisyA
    return {"inds" : sorted(noisePruned), "name" : "PruneSF"}
    #return noisePruned
    
    
# to ARFF String

def toARFFstring(table,try_numericize=0):#filename,table,try_numericize=0):
    import cStringIO, string    
    t = table
    #if filename[-5:] == ".arff":
     #   filename = filename[:-5]
    #print filename
    f = cStringIO.StringIO()
    f.write('@relation %s\n'%t.domain.classVar.name)
    # attributes
    ats = [i for i in t.domain.attributes]
    ats.append(t.domain.classVar)
    for i in ats:
        real = 1
        if i.varType == 1:
            if try_numericize:
                # try if all values numeric
                for j in i.values:
                    try:
                        x = string.atof(j)
                    except:
                        real = 0 # failed
                        break
            else:
                real = 0
        iname = str(i.name)
        if string.find(iname," ") != -1:
            iname = "'%s'"%iname
        if real==1:
            f.write('@attribute %s real\n'%iname)
        else:
            f.write('@attribute %s { '%iname)
            x = []
            for j in i.values:
                s = str(j)
                if string.find(s," ") == -1:
                    x.append("%s"%s)
                else:
                    x.append("'%s'"%s)
            for j in x[:-1]:
                f.write('%s,'%j)
            f.write('%s }\n'%x[-1])

    # examples
    f.write('@data\n')
    for j in t:
        x = []
        for i in range(len(ats)):
            s = str(j[i])
            if string.find(s," ") == -1:
                x.append("%s"%s)
            else:
                x.append("'%s'"%s)
        for i in x[:-1]:
            f.write('%s,'%i)
        f.write('%s\n'%x[-1])
        
    return f

def getWekaName(name):
    #print name
    if name == None:
        return 'Multilayer Perceptron (Weka)'
    elif name.startswith('No '):
        return 'J48 (Weka)'
    elif name.startswith('Naive Bayes') or name.startswith('Random forest'):
        return "".join([name.split()[0], ' ', name.split()[1], ' (Weka)'])
    else:
        return name.split()[0].rstrip(':') + ' (Weka)'