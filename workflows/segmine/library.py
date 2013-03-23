'''
Segmine library.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
#
# Visualization widgets:
#
def segmine_rank_plotter(input_dict):
    return input_dict

def segmine_rule_browser(input_dict):
    return input_dict

#
# Interactions widgets:
#
def segmine_fc_gene_filter_finished(postdata, input_dict, output_dict):
    from orngBioinformatics import obiExpression as rankers
    import orange
    dataset = input_dict['dataset']
    widget_id = postdata.get('widget_id')[0]
    fc_threshold = float(postdata.get('fc_threshold%s' % widget_id)[0])
    targets = map(str, postdata.get('target%s' % widget_id))
    ranker = rankers.ExpressionSignificance_FoldChange(dataset, False)
    ranks = ranker(target=targets if len(targets)>1 else targets[0])
    new_domain = orange.Domain([att for att, fc in ranks if fc >= fc_threshold], dataset.domain.classVar)
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
    new_domain = orange.Domain([att for att, (t, pval) in ranks if pval <= pvalue_threshold], dataset.domain.classVar)
    reduced_dataset = orange.ExampleTable(new_domain, dataset)
    return {'dataset' : reduced_dataset}

#
# Regular widgets:
#
def segmine_ttest_gene_filter(input_dict):
    return {'dataset' : None}

def segmine_fc_gene_filter(input_dict):
    return {'dataset' : None}

def segmine_gene_ranker(input_dict):
    import orange
    from numpy import mean, var
    from math import sqrt
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
    for attr in table.domain.attributes:
        geneID = attr.name

        controlValues = [float(example[attr]) for example in control]

        dataValues = [float(example[attr]) for example in data]

        try:
            tScores[geneID] = (mean(dataValues) - mean(controlValues)) / sqrt(var(controlValues)/len(controlValues) + var(dataValues)/len(dataValues))
        except ZeroDivisionError:
            tScores[geneID] = 0.0

    sortedTScores = [(elt[1], elt[0])  for elt in sorted([(tScores[attr.name], attr.name)  for attr in table.domain.attributes], reverse=True)]

    return {'geneRanks':geneRanks,'tScores':sortedTScores}

def segmine_segs(input_dict):
    # TODO
    output_dict = {}
    return output_dict

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