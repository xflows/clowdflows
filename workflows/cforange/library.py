def cforange_split_dataset(input_dict):
    import orange
    output_dict = {}
    data = input_dict['dataset']
    selection = orange.MakeRandomIndices2(data,float(input_dict['p']))
    train_data = data.select(selection,0)
    test_data = data.select(selection,1)
    output_dict['train_data']=train_data
    output_dict['test_data']=test_data
    return output_dict

def cforange_score_estimation(input_dict):
    import orange
    import orngFSS
    data = input_dict['dataset']
    ma = orngFSS.attMeasure(data,orange.MeasureAttribute_relief(k=int(input_dict['k']), m=int(input_dict['m'])))
    output_string = ""
    output_dict = {}
    output_dict['results'] = ma
    return output_dict

def cforange_best_natts(input_dict):
    import orange
    import orngFSS
    data = input_dict['dataset']
    scores = input_dict['scores']
    n = int(input_dict['n'])
    new_dataset = orngFSS.selectBestNAtts(data,scores,n)
    output_dict={}
    output_dict['new_dataset'] = new_dataset
    return output_dict

def cforange_atts_above_thresh(input_dict):
    import orange
    import orngFSS
    data = input_dict['dataset']
    scores = input_dict['scores']
    thresh = float(input_dict['thresh'])
    new_dataset = orngFSS.selectAttsAboveThresh(data,scores,thresh)
    output_dict={}
    output_dict['new_dataset'] = new_dataset
    return output_dict

def cforange_filter_relieff(input_dict):
    import orange
    import orngFSS
    data = input_dict['dataset']
    measure = orange.MeasureAttribute_relief(k=int(input_dict['k']), m=int(input_dict['m']))
    margin = float(input_dict['margin'])
    new_dataset = orngFSS.filterRelieff(data,measure,margin)
    output_dict = {}
    output_dict['new_dataset'] = new_dataset
    return output_dict   

def cforange_multiple_cross_validation(input_dict):
    import orange, orngTest, orngStat
    learners = input_dict['learners']
    data = input_dict['dataset']
    folds = int(input_dict['folds'])
    results = orngTest.crossValidation(learners, data, folds=folds)
    output_dict = {}
    output_dict['results']=results
    return output_dict

def cforange_proportion_test(input_dict):
    import orange, orngTest, orngStat
    learners = input_dict['learners']
    data = input_dict['dataset']
    learnProp = float(input_dict['learnProp'])
    times = int(input_dict['times'])
    results = orngTest.proportionTest(learners, data, learnProp, times=times)
    output_dict = {}
    output_dict['results']=results
    return output_dict  

def cforange_leave_one_out(input_dict):
    import orange, orngTest, orngStat
    learners = input_dict['learners']
    data = input_dict['dataset']
    results = orngTest.leaveOneOut(learners, data)
    output_dict = {}
    output_dict['results']=results
    return output_dict       

def cforange_cross_validation(input_dict):
    import orange, orngTest, orngStat
    learners = [input_dict['learner']]
    data = input_dict['dataset']
    folds = int(input_dict['folds'])
    results = orngTest.crossValidation(learners, data, folds=folds)
    output_dict = {}
    output_dict['results']=results
    return output_dict

def cforange_classification_accuracy(input_dict):
    import orngStat
    results = input_dict['results']
    CAs = orngStat.CA(results)
    if len(CAs)==1:
        CAs = CAs[0]
    output_dict = {}
    output_dict['ca']=CAs
    return output_dict

def cforange_classification_accuracy(input_dict):
    import orngStat
    results = input_dict['results']
    if input_dict['reportSE']=='true':
        reportSE = True
    else:
        reportSE = False
    CAs = orngStat.CA(results,reportSE=reportSE)
    if len(CAs)==1:
        CAs = CAs[0]
    output_dict = {}
    output_dict['ca']=CAs
    return output_dict 

def cforange_average_probability(input_dict):
    import orngStat
    results = input_dict['results']
    if input_dict['reportSE']=='true':
        reportSE = True
    else:
        reportSE = False
    APs = orngStat.AP(results,reportSE=reportSE)
    if len(APs)==1:
        APs = APs[0]
    output_dict = {}
    output_dict['ap']=APs
    return output_dict

def cforange_brier_score(input_dict):
    import orngStat
    results = input_dict['results']
    if input_dict['reportSE']=='true':
        reportSE = True
    else:
        reportSE = False
    BSs = orngStat.BrierScore(results,reportSE=reportSE)
    if len(BSs)==1:
        BSs = BSs[0]
    output_dict = {}
    output_dict['bs']=BSs
    return output_dict 

def cforange_information_score(input_dict):
    import orngStat
    results = input_dict['results']
    if input_dict['reportSE']=='true':
        reportSE = True
    else:
        reportSE = False
    ISs = orngStat.IS(results,apriori=None,reportSE=reportSE)
    if len(ISs)==1:
        ISs = ISs[0]
    output_dict = {}
    output_dict['is']=ISs
    return output_dict

def cforange_confusion_matrix(input_dict):
    import orngStat
    results = input_dict['results']
    classIndex = int(input_dict['classIndex'])
    if input_dict['cutoff']!='':
        cutoff = float(input_dict['cutoff'])
        cm = orngStat.confusionMatrices(results,classIndex=classIndex,cutoff=cutoff)
    else:
        cm = orngStat.confusionMatrices(results,classIndex=classIndex)
    if len(cm)==1:
        cm = cm[0]
    print cm
    output_dict = {}
    output_dict['cm']=cm
    return output_dict

def cforange_confusion_matrix_computations(input_dict):
    import orngStat
    cm = input_dict['cm']
    alpha = float(input_dict['alpha'])
    output_dict = {}
    output_dict['sens']=orngStat.sens(cm)
    output_dict['spec']=orngStat.spec(cm)
    output_dict['PPV']=orngStat.PPV(cm)
    output_dict['NPV']=orngStat.NPV(cm)
    output_dict['precision']=orngStat.precision(cm)
    output_dict['recall']=orngStat.recall(cm)
    output_dict['F1']=orngStat.F1(cm)
    output_dict['Falpha']=orngStat.Falpha(cm,alpha=alpha)
    output_dict['MCC']=orngStat.MCC(cm)
    return output_dict  

def cforange_auc(input_dict):
    import orngStat
    results = input_dict['results']
    method = int(input_dict['method'])
    auc = orngStat.AUC(results,method)
    output_dict = {}
    output_dict['AUC']=auc
    return output_dict  