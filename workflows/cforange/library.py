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

def cforange_MSE(input_dict):
    import orngStat
    results = input_dict['results']
    errors = orngStat.MSE(results)
    if len(errors)==1:
        errors = errors[0]
    output_dict = {}
    output_dict['MSE']=errors
    return output_dict

def cforange_RMSE(input_dict):
    import orngStat
    results = input_dict['results']
    errors = orngStat.RMSE(results)
    if len(errors)==1:
        errors = errors[0]
    output_dict = {}
    output_dict['RMSE']=errors
    return output_dict    

def cforange_MAE(input_dict):
    import orngStat
    results = input_dict['results']
    errors = orngStat.MAE(results)
    if len(errors)==1:
        errors = errors[0]
    output_dict = {}
    output_dict['MAE']=errors
    return output_dict

def cforange_RSE(input_dict):
    import orngStat
    results = input_dict['results']
    errors = orngStat.RSE(results)
    if len(errors)==1:
        errors = errors[0]
    output_dict = {}
    output_dict['RSE']=errors
    return output_dict    

def cforange_RRSE(input_dict):
    import orngStat
    results = input_dict['results']
    errors = orngStat.RRSE(results)
    if len(errors)==1:
        errors = errors[0]
    output_dict = {}
    output_dict['RRSE']=errors
    return output_dict    

def cforange_RAE(input_dict):
    import orngStat
    results = input_dict['results']
    errors = orngStat.RAE(results)
    if len(errors)==1:
        errors = errors[0]
    output_dict = {}
    output_dict['RAE']=errors
    return output_dict    

def cforange_R2(input_dict):
    import orngStat
    results = input_dict['results']
    errors = orngStat.R2(results)
    if len(errors)==1:
        errors = errors[0]
    output_dict = {}
    output_dict['R2']=errors
    return output_dict        

def cforange_prepare_results(input_dict):
    output_dict = {}
    learners = input_dict['learners']
    newlist = []
    for i in range(0,len(learners)):
        newdict = {}
        newdict['name']=str(learners[i])
        newdict['fbeta']=0.5
        newdict['precision']=input_dict['precision'][i]
        newdict['recall']=input_dict['recall'][i]
        newdict['fscore']=input_dict['f'][i]
        newlist.append(newdict)
    output_dict['alp']=newlist
    return output_dict

def cforange_example_distance(input_dict):
    import orange
    import random
    import orngClustering
    import orngMisc
    inputdata = input_dict['dataset']
    metricsIndex = int(input_dict['distanceMetrics'])
    metrics = [
        ("Euclidean", orange.ExamplesDistanceConstructor_Euclidean),
        ("Pearson Correlation", orngClustering.ExamplesDistanceConstructor_PearsonR),
        ("Spearman Rank Correlation", orngClustering.ExamplesDistanceConstructor_SpearmanR),
        ("Manhattan", orange.ExamplesDistanceConstructor_Manhattan),
        ("Hamming", orange.ExamplesDistanceConstructor_Hamming),
        ("Relief", orange.ExamplesDistanceConstructor_Relief),
        ]

    normalize = input_dict['normalization']
    if normalize=='true':
        normalize = True
    else:
        normalize = False

    data = inputdata
    constructor = metrics[metricsIndex][1]()
    constructor.normalize = normalize
    dist = constructor(data)
    matrix = orange.SymMatrix(len(data))
    matrix.setattr('items', data)
    for i in range(len(data)):
        for j in range(i+1):
            matrix[i, j] = dist(data[i], data[j])
    output_dict = {}
    output_dict['dm']=matrix
    return output_dict

def cforange_discretize(input_dict):
    import orange
    from collections import defaultdict

    input_obj = input_dict['dataset']
    intervals = input_dict['intervals']
    output_tables = []

    input_type = input_obj.__class__.__name__

    if input_type == 'DBContext':
        context = input_obj
        input_tables = [context.orng_tables[tname] for tname in context.tables]
    elif input_type != 'list':
        input_tables = [input_obj]
    else:
        input_tables = input_obj

    discretizerIndex = int(input_dict['discretizer_id'])
    discretizers = [
        ("Equi-distant discretization",orange.EquiDistDiscretization), #numberOfIntervals
        ("Quantile-based discretization", orange.EquiNDiscretization), #numberOfIntervals
        ("Entropy-based discretization", orange.EntropyDiscretization), #no arguments
        ("Bi-modal discretization", orange.BiModalDiscretization),#no arguments
        ("Fixed discretization", orange.EquiNDiscretization)#FixedDiscretization) #points
    ]

    options = {}
    points = defaultdict(dict)
    if not intervals:
        if discretizerIndex in [4]:
            #find all cut-off points
            user_points = [float(a) for a in input_dict['points'].replace(" ","").split(",")]
            options['points']=sorted(user_points)
        elif discretizerIndex in [0,1]:
            options['numberOfIntervals']=int(input_dict['numberOfIntervals'])

    d = discretizers[discretizerIndex][1](**options)

    for inputdata in input_tables:
        newattrs = []
        for attr in inputdata.domain.attributes:
            if attr.varType == orange.VarTypes.Continuous:

                if not intervals:
                    # No intervals provided, calculate them
                    newattr = d(attr,inputdata) if discretizerIndex in [0,2,3] else d.constructVariable(attr)
                else:
                    # Use the input interval dictionary
                    attr_points = intervals[inputdata.name][attr.name]
                    idisc = orange.IntervalDiscretizer(points = attr_points)
                    newattr = idisc.constructVariable(attr)

                newattr.name = attr.name
                newattrs.append(newattr)
                points[inputdata.name][attr.name] = newattr.get_value_from.transformer.points
            else:
                newattrs.append(attr)
        name = inputdata.name
        newdomain = orange.Domain(newattrs, inputdata.domain.classVar)
        newdomain.addmetas(inputdata.domain.getmetas())
        new_t = orange.ExampleTable(newdomain, inputdata)
        new_t.name = name
        output_tables.append(new_t)

    if input_type == 'DBContext':
        output = input_obj.copy()
        output.orng_tables = dict(zip(input_obj.tables, output_tables))
    elif input_type != 'list':
        output = output_tables[0]
    else:
        output = output_tables

    output_dict = {'odt': output, 'discr_intervals': points}
    return output_dict

def cforange_attribute_distance(input_dict):
    import orange
    import orngInteract
    inputdata = input_dict['dataset']
    discretizedData = None
    classInteractions = int(input_dict['classInteractions'])
    atts = inputdata.domain.attributes
    if len(atts) < 2:
        return None
    matrix = orange.SymMatrix(len(atts))
    matrix.setattr('items', atts)
    if classInteractions < 3:
        if inputdata.domain.hasContinuousAttributes():
            if discretizedData is None:
                try:
                    discretizedData = orange.Preprocessor_discretize(inputdata, method=orange.EquiNDiscretization(numberOfIntervals=4))
                except orange.KernelException, ex:
                    return None
            data = discretizedData
        else:
            data = inputdata

        # This is ugly (no shit)
        if not data.domain.classVar:
            if classInteractions == 0:
                classedDomain = orange.Domain(data.domain.attributes, orange.EnumVariable("foo", values=["0", "1"]))
                data = orange.ExampleTable(classedDomain, data)
            else:
                return None

        im = orngInteract.InteractionMatrix(data, dependencies_too=1)
        off = 1
        if classInteractions == 0:
            diss,labels = im.exportChi2Matrix()
            off = 0
        elif classInteractions == 1:
            (diss,labels) = im.depExportDissimilarityMatrix(jaccard=1)  # 2-interactions
        else:
            (diss,labels) = im.exportDissimilarityMatrix(jaccard=1)  # 3-interactions

        for i in range(len(atts)-off):
            for j in range(i+1):
                matrix[i+off, j] = diss[i][j]

    else:
        if classInteractions == 3:
            for a1 in range(len(atts)):
                for a2 in range(a1):
                    matrix[a1, a2] = (1.0 - orange.PearsonCorrelation(a1, a2, inputdata, 0).r) / 2.0
        else:
            if len(inputdata) < 3:
                return None
            import numpy, statc
            m = inputdata.toNumpyMA("A")[0]
            averages = numpy.ma.average(m, axis=0)
            filleds = [list(numpy.ma.filled(m[:,i], averages[i])) for i in range(len(atts))]
            for a1, f1 in enumerate(filleds):
                for a2 in range(a1):
                    matrix[a1, a2] = (1.0 - statc.spearmanr(f1, filleds[a2])[0]) / 2.0
    output_dict = {}
    output_dict['dm']=matrix        
    return output_dict

def cforange_hierarchical_clustering(input_dict):
    return {'centroids' : None, 'selected_examples' : None, 'unselected_examples' : None}

class Clustering:
    @staticmethod
    def hierarchical_clustering(linkage, distance_matrix):
        import Orange, orange, sys
        linkages = [("Single linkage", orange.HierarchicalClustering.Single),
                    ("Average linkage", orange.HierarchicalClustering.Average),
                    ("Ward's linkage", orange.HierarchicalClustering.Ward),
                    ("Complete linkage", orange.HierarchicalClustering.Complete)]
        try:
            return orange.HierarchicalClustering(distance_matrix, linkage=linkages[linkage][1])
        except TypeError as e:
            print "hierarchical_clustering:", sys.exc_info()[0]
            print e
            #raise

def cforange_hierarchical_clustering_finished(postdata, input_dict, output_dict):
    print "cforange_hierarchical_clustering_finished"
    import json
    import Orange, orange
    
    matrix = input_dict['dm']
    linkage = int(input_dict['linkage'])
    widget_pk = postdata['widget_id'][0]

    try:
        selected_nodes = json.loads(postdata.get('selected_nodes')[0])
    except:
        raise Exception('Please select a threshold for determining clusters.')
    if isinstance(matrix.items, orange.ExampleTable):
        root = Clustering.hierarchical_clustering(linkage, matrix)
        cluster_ids = set([cluster for _,_,cluster in selected_nodes])
        selected_clusters = set([cluster for _,selected,cluster in selected_nodes if selected])
        clustVar = orange.EnumVariable(str('Cluster'), values=["Cluster %d" % i for i in cluster_ids] + ["Other"])
        origDomain = matrix.items.domain
        domain = orange.Domain(origDomain.attributes, origDomain.classVar)
        domain.addmeta(orange.newmetaid(), clustVar)
        domain.addmetas(origDomain.getmetas())
        # Build table with selected clusters
        selected_table, unselected_table = orange.ExampleTable(domain), orange.ExampleTable(domain)
        for id, selected, cluster in selected_nodes:
            new_ex = orange.Example(domain, matrix.items[id])
            if selected:
                new_ex[clustVar] = clustVar("Cluster %d" % cluster)
                selected_table.append(new_ex)
            else:
                new_ex[clustVar] = clustVar("Other")
                unselected_table.append(new_ex)
        # Build table of centroids
        centroids = orange.ExampleTable(selected_table.domain)
        if len(selected_table) > 0:
            for cluster in sorted(selected_clusters):
                clusterEx = orange.ExampleTable([ex for ex in selected_table if ex[clustVar] == "Cluster %d" % cluster])
                # Attribute statistics
                contstat = orange.DomainBasicAttrStat(clusterEx)
                discstat = orange.DomainDistributions(clusterEx, 0, 0, 1)
                ex = [cs.avg if cs else (ds.modus() if ds else "?") for cs, ds in zip(contstat, discstat)]
                example = orange.Example(centroids.domain, ex)
                example[clustVar] = clustVar("Cluster %d" % cluster)
                centroids.append(example)
    else: # Attribute distance
        centroids, selected_table, unselected_table = None, None, None
    return {'centroids' : centroids, 'selected_examples' : selected_table, 'unselected_examples' : unselected_table}

def cforange_odt_to_kdic(input_dict):
    from odt_converters import toKDICstring, toKDICheader
    output_dict = {}
    f = toKDICheader(input_dict['odt'])
    output_dict['kdic'] = f.getvalue()
    f2 = toKDICstring(input_dict['odt'])
    output_dict['txt'] = f2.getvalue()
    return output_dict  

def cforange_odt_to_prd_fct(input_dict):
    from odt_converters import toPRDstring, toFCTstring
    output_dict = {}
    f = toPRDstring(input_dict['odt'])
    output_dict['prd'] = f.getvalue()
    f2 = toFCTstring(input_dict['odt'])
    output_dict['fct'] = f2.getvalue()
    return output_dict  

def filter_table(input_dict):
    return {'altered_data' : None}

def filter_table_finished(postdata, input_dict, output_dict):
    print postdata
    import Orange
    from Orange.feature import Type
    widget_id = postdata['widget_id'][0]
    # Parse the changes
    new_table = Orange.data.Table(input_dict['data']).getitems([int(x) for x in postdata.get('include',[])])
    return {'altered_data' : new_table}
