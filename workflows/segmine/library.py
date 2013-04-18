'''
Segmine library.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from biomine import BiomineSearch
from segs import Segs

#
# Visualization widgets:
#
def segmine_rank_plotter(input_dict):
    return {}

def segmine_biomine_visualizer(input_dict):
    return {'graph' : input_dict.get('graph', None)}

#
# Interactions widgets:
#
def segmine_rule_browser(input_dict):
    return {'node_list' : []}

def segmine_rule_browser_finished(input_dict):
    return {'node_list' : []}

def segmine_fc_gene_filter_finished(postdata, input_dict, output_dict):
    from orngBioinformatics import obiExpression as rankers
    import orange
    dataset = input_dict['dataset']
    widget_id = postdata.get('widget_id')[0]
    fc_threshold = float(postdata.get('fc_threshold%s' % widget_id)[0])
    targets = map(str, postdata.get('target%s' % widget_id))
    ranker = rankers.ExpressionSignificance_FoldChange(dataset, False)
    ranks = ranker(target=targets if len(targets)>1 else targets[0])
    new_domain = orange.Domain([att for att, fc in ranks if fc >= fc_threshold], 
                               dataset.domain.classVar)
    reduced_dataset = orange.ExampleTable(new_domain, dataset)
    return {'dataset' : reduced_dataset}

def segmine_ttest_gene_filter_finished(postdata, input_dict, output_dict):
    from orngBioinformatics import obiExpression as rankers
    import orange
    dataset = input_dict['dataset']
    widget_id = postdata.get('widget_id')[0]
    pvalue_threshold = float(postdata.get('pvalue_threshold%s' % widget_id)[0])
    targets = map(str, postdata.get('target%s' % widget_id))
    ranker = rankers.ExpressionSignificance_TTest(dataset, False)
    ranks = ranker(target=targets if len(targets)>1 else targets[0])
    filter_atts = [att for att, (t, pval) in ranks if pval <= pvalue_threshold]
    new_domain = orange.Domain(filter_atts, dataset.domain.classVar)
    reduced_dataset = orange.ExampleTable(new_domain, dataset)
    return {'dataset' : reduced_dataset}

#
# Regular widgets:
#
def segmine_ttest_gene_filter(input_dict):
    return {'dataset' : None}

def segmine_fc_gene_filter(input_dict):
    return {'dataset' : None}

def segmine_gene_ranker(input_dict, widget):
    import orange
    from numpy import mean, var
    from math import sqrt, floor
    CONTROL_GROUP_KEY = 'control group'
    DATA_GROUP_KEY = 'data group'
    CLASS_ATRR_NAME = 'group'    
    table = input_dict['microarrayTable']
    k = int(input_dict['k'])
    m = int(input_dict['m'])
    if m == 0: # special value
        m= -1  # all examples
    ranks = []
    # ReliefF parameters:
    #  - number of neighbours: 10
    #  - number of reference examples: all (-1)
    #  - checksum computation: none (the data do not change)
    ranker = orange.MeasureAttribute_relief(k=k, m=m, checkCachedData=False)
    for attr in table.domain.attributes:
        ranks.append((ranker(attr, table), attr.name))

    # tuples are sorted according to the first element,
    # here, this is attribute's quality
    ranks.sort(reverse=True)

    # reverse order inside sorted tuples list in result
    geneRanks = [(elt[1], elt[0]) for elt in ranks]
    tScores = {}
    control = table.selectref({CLASS_ATRR_NAME:CONTROL_GROUP_KEY})
    data = table.selectref({CLASS_ATRR_NAME:DATA_GROUP_KEY})
    nerrors = 0
    widget.progress = 0
    widget.save()
    for i, attr in enumerate(table.domain.attributes):
        geneID = attr.name
        controlValues = [float(example[attr]) for example in control]
        dataValues = [float(example[attr]) for example in data]
        try:
            average = mean(dataValues) - mean(controlValues)
            variance = var(controlValues)/len(controlValues) + \
                       var(dataValues)/len(dataValues)
            score = average/sqrt(variance)
            tScores[geneID] = score
        except ZeroDivisionError:
            tScores[geneID] = 0.0
        if (i + 1)%100 == 0:
            widget.progress = floor((i + 1)/float(len(table.domain.attributes))*100)
            widget.save()
    widget.progress = 100
    widget.save()
    sortedTScores = sorted(tScores.items(), reverse=True, key=lambda x: x[1])
    return {'geneRanks':geneRanks,'tScores':sortedTScores}

def segmine_segs(input_dict, widget):
    segs = Segs(input_dict['wsdl'])
    results = segs.run(input_dict, widget=widget)
    return {'rules_fisher' : results.get('rules_fisher', None),
            'rules_PAGE' : results.get('rules_PAGE', None),
            'rules_GSEA' : results.get('rules_GSEA', None),
            'rules_combined' : results.get('rules_combined', None)}

def segmine_resolve_gene_synonyms(input_dict):
    from .data import mappings
    gene_ranks = input_dict['gene_ranks']
    unknown = 0
    ndup = 0
    mapped = []
    genes = {}
    for (i, (geneID, rank)) in enumerate(gene_ranks):
        if i%100 == 0:
            self.progressBar.advance()
        # gene name can also be symbolic or synonym
        geneID = geneID.lower()
        try:
            entrezID = int(geneID)
        except ValueError:
            if geneID in mappings.symbol2entrez:
                entrezID = mappings.symbol2entrez[geneID]
            elif geneID in mappings.synonyms2entrez:
                entrezID = mappings.synonyms2entrez[geneID]
            else:
                unknown += 1
                continue
        if entrezID in genes:
            ndup += 1
            continue
        else:
            mapped.append((str(entrezID), rank))
            genes[entrezID] = None
    return {'gene_ranks' : mapped}

def segmine_biomine_neighbourhood(input_dict):
    groupNodes = input_dict.get('groupNodes', False)
    singleComponent = input_dict.get('singleComponent', False)
    maxNodes = int(input_dict.get('maxNodes', 0))
    startNodes = input_dict.get('startNodes', None)
    databaseVersion = input_dict.get('databaseVersion')

    search = BiomineSearch(groupNodes=groupNodes, 
                           singleComponent=singleComponent,
                           maxNodes=maxNodes,
                           startNodes=startNodes,
                           databaseVersion=databaseVersion)
    result, bestPath = search.invokeBiomine()
    return {'result' : result, 'bestPath' : bestPath}

def segmine_biomine_connection(input_dict):
    groupNodes = input_dict.get('groupNodes', False)
    singleComponent = input_dict.get('singleComponent', False)
    maxNodes = int(input_dict.get('maxNodes', 0))
    startNodes = input_dict.get('startNodes', None)
    endNodes = input_dict.get('endNodes', None)
    databaseVersion = input_dict.get('databaseVersion')

    search = BiomineSearch(groupNodes=groupNodes, 
                           singleComponent=singleComponent,
                           maxNodes=maxNodes,
                           startNodes=startNodes,
                           endNodes=endNodes,
                           databaseVersion=databaseVersion)
    result, bestPath = search.invokeBiomine()
    return {'result' : result, 'bestPath' : bestPath}

def segmine_biomine_medoid(input_dict):
    groupNodes = input_dict.get('groupNodes', False)
    singleComponent = input_dict.get('singleComponent', False)
    maxNodes = int(input_dict.get('maxNodes', 0))
    startNodes = input_dict.get('startNodes', None)
    databaseVersion = input_dict.get('databaseVersion')

    search = BiomineSearch(groupNodes=groupNodes, 
                           singleComponent=singleComponent,
                           maxNodes=maxNodes,
                           medoids=True,
                           startNodes=startNodes,
                           databaseVersion=databaseVersion)
    result, bestPath = search.invokeBiomine()
    return {'result' : result, 'bestPath' : bestPath}

