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

def cforange_cross_validation(input_dict):
    import orange, orngTest, orngStat
    learners = input_dict['learners']
    data = input_dict['dataset']
    folds = int(input_dict['folds'])
    results = orngTest.crossValidation(learners, data, folds=folds)
    output_dict = {}
    output_dict['results']=results
    return output_dict