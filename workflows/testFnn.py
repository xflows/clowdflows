import orange, orngTree, random
from pybrain.datasets import ClassificationDataSet

def fnn():
    data = orange.ExampleTable("D:\\Back-up-THICK_on_Vista\\Orange\\W1BIN.tab")#input_dict['data'])
    addMetaID(data)
    n_attrs = len(data.domain.attributes)
    classes = list(data.domain.classVar.values)
    pbdata = ClassificationDataSet(n_attrs, class_labels=classes)
    for ex in data:
        pbdata.appendLinked([x.value for x in list(ex)[:n_attrs]], [classes.index(ex.getclass().value)])
        
    tstdata, trndata = pbdata.splitWithProportion( 0.25 )
    trndata._convertToOneOfMany( )
    tstdata._convertToOneOfMany( )
    print "Number of training patterns: ", len(trndata)
    print "Input and output dimensions: ", trndata.indim, trndata.outdim
    print "First sample (input, target, class):"
    print trndata['input'][0], trndata['target'][0], trndata['class'][0]
        
    

def addMetaID(data):
    meta_id = orange.FloatVariable("meta_id")
    id = orange.newmetaid()
    data.domain.addmeta(id, meta_id)
    for i in range(len(data)):
        data[i][meta_id] = i

def cf(input_dict):
#    tempfile = open("tempds.tab", 'w')
#    tempfile.write(input_dict['data'])
#    tempfile.close()
#    data = orange.ExampleTable("tempds.tab")
    data = orange.ExampleTable(input_dict['data'])
    addMetaID(data)
    k = 10
    noisyIndices = []
    selection = orange.MakeRandomIndicesCV(data, folds=k)
    count_noisy = [0]*k
    for test_fold in range(k):
        train_data = data.select(selection, test_fold, negate=1)
        test_data = data.select(selection, test_fold)
        #print "\t\t", "Learned on", len(train_data), "examples"
        #file.flush()
        classifier = input_dict['learner'](train_data)
        for example in test_data:
            if classifier(example) != example.getclass():
                # selection_filter[int(example[meta_id].value)] = 0
                noisyIndices.append(int(example["meta_id"].value))
                count_noisy[test_fold] += 1
        # END test_data
    # END test_fold
    return noisyIndices
    ##    filtered_data = data.select(selection_filter, 1)
    ##    noisy_data = data.select(selection_filter, 0)
    ##    return [filtered_data, noisy_data]=======

fnn()