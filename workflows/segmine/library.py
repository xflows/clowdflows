'''
Segmine library.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from biomine import BiomineSearch
from segs import Segs
import logging


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

def segmine_rule_browser_finished(postdata, input_dict, output_dict):
    rules = input_dict['rules']
    widget_id = postdata.get('widget_id')[0]
    selectedCell = postdata.get('selectedCell')[0]
    node_list = []
    if selectedCell:
        key, _, idx = selectedCell.split('_')
        rule = rules[int(idx)]
        if key == 'terms':
            terms = rule['description']['terms'] + \
                    rule['description']['interactingTerms']
            node_list.extend([term['termID'] for term in terms])
        elif key in ['coveredGenes', 'coveredTopGenes']:
            genes = ['EntrezGene:%s' % gene for gene in rule[key]]
            node_list.extend(genes)
    return {'node_list' : node_list}

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
    return {'geneRanks': geneRanks, 'tScores': sortedTScores}


def segmine_segs(input_dict, widget):
    segs = Segs(input_dict['wsdl'])
    results = segs.run(input_dict, widget=widget)
    return {'rules_fisher' : results.get('rules_fisher', None),
            'rules_PAGE' : results.get('rules_PAGE', None),
            'rules_GSEA' : results.get('rules_GSEA', None),
            'rules_combined' : results.get('rules_combined', None)}


def segmine_segs_stu(input_dict, widget):
    segs = Segs(input_dict['wsdl'])
    results = segs.run_STU(input_dict, widget=widget)
    return {'rules_fisher': results.get('rules_fisher', None),
            'rules_PAGE': results.get('rules_PAGE', None),
            'rules_GSEA': results.get('rules_GSEA', None),
            'rules_combined': results.get('rules_combined', None)}


def segmine_segs_ath(input_dict, widget):
    segs = Segs(input_dict['wsdl'])
    results = segs.run_ATH(input_dict, widget=widget)
    return {'rules_fisher': results.get('rules_fisher', None),
            'rules_PAGE': results.get('rules_PAGE', None),
            'rules_GSEA': results.get('rules_GSEA', None),
            'rules_combined': results.get('rules_combined', None)}


def segmine_resolve_gene_synonyms(input_dict):
    from .data import mappings
    gene_ranks = input_dict['gene_ranks']
    unknown = 0
    ndup = 0
    mapped = []
    genes = {}
    for (i, (geneID, rank)) in enumerate(gene_ranks):
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
    return {'gene_ranks': mapped}


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


def segmine_mirna_to_gene_tarbase(input_dict):
    import cPickle
    from os.path import normpath, join, dirname

    mirna_ranks = input_dict['mirna_ranks']
    mirna2gene = cPickle.load(open(normpath(join(dirname(__file__), 'data/mirna2gene_tarbase')),'rb'))

    result = {}
    unknown = 0
    for (rna, rank) in mirna_ranks:
        rna = rna.lower()
        if rna not in mirna2gene:
            unknown += 1
            continue
        for gene in mirna2gene[rna]:
            if gene not in result:
                result[gene] = rank
            else:
                result[gene] += rank
    #end
    # if unknown:
    #     self.warning('%d unknown miRNA were found and ignored!' % unknown)

    result = sorted([(pair[1], pair[0]) for pair in result.items()], reverse=True)
    result = [(str(pair[1]), pair[0]) for pair in result]
    return {'gene_ranks': result}
#end


def segmine_mirna_to_gene_targetscan(input_dict):
    import cPickle
    from os.path import normpath, join, dirname

    mirna_ranks = input_dict['mirna_ranks']
    mirna2gene = cPickle.load(open(normpath(join(dirname(__file__), 'data/mirna2gene_targetscan')),'rb'))

    result = {}
    unknown = 0
    for (rna, rank) in mirna_ranks:
        rna = rna.lower()
        if rna not in mirna2gene:
            unknown += 1
            continue
        for gene in mirna2gene[rna]:
            if gene not in result:
                result[gene] = rank
            else:
                result[gene] += rank
    #end
    # if unknown:
    #     self.warning('%d unknown miRNA were found and ignored!' % unknown)

    result = sorted([(pair[1], pair[0]) for pair in result.items()], reverse=True)
    result = [(str(pair[1]), pair[0]) for pair in result]
    return {'gene_ranks': result}
#end


def __makeExampleTable(namesDict, data):
    import orange
    from constants import CLASS_ATRR_NAME, CONTROL_GROUP_KEY, DATA_GROUP_KEY

    geneIDs = sorted(data.keys())
    attrList = [orange.FloatVariable(name=str(geneID)) for geneID in geneIDs]
    classAttr = orange.EnumVariable(name=CLASS_ATRR_NAME, values = [CONTROL_GROUP_KEY, DATA_GROUP_KEY])
    domain = orange.Domain(attrList, classAttr)
    table = orange.ExampleTable(domain)

    # first half: group 1
    for attrName in namesDict[CONTROL_GROUP_KEY].keys():
        exampleValues = [data[geneID][CONTROL_GROUP_KEY][attrName] for geneID in geneIDs] + [CONTROL_GROUP_KEY]
        example = orange.Example(domain, exampleValues)
        table.append(example)

    # second half: group 2
    for attrName in namesDict[DATA_GROUP_KEY].keys():
        exampleValues = [data[geneID][DATA_GROUP_KEY][attrName] for geneID in geneIDs] + [DATA_GROUP_KEY]
        example = orange.Example(domain, exampleValues)
        table.append(example)

    return table
#end


def segmine_read_microarray_data(input_dict):
    from numpy import mean
    import math
    from constants import CLASS_ATRR_NAME, CONTROL_GROUP_KEY, DATA_GROUP_KEY, DEFAULT_CONTROL_GROUP_ID

    data = open(input_dict['file']).read()
    dataFormat = 'linear' if int(input_dict['idf']) == 1 else 'log2'
    calcMethod = 'ratio' if int(input_dict['cm']) == 1 else 'difference'

    lines = [x.replace(',', ' ').split() for x in data.splitlines()]
    names = lines[0][1:] # skip name of gene column

    # find the prefix of the data channel (the first group prefix is fixed in advance)
    pfs = set()
    for name in names:
        pfs.add(name[0])
    if len(pfs) != 2:
        raise ValueError('Invalid data header: more than two prefixes found: %s' % str(list(pfs)))

    # if the data do not obey the default rule, the first character of the first column
    # is the identifier of the first group
    if DEFAULT_CONTROL_GROUP_ID not in pfs:
        CONTROL_GROUP_ID = names[0][0]
    else:
        CONTROL_GROUP_ID = DEFAULT_CONTROL_GROUP_ID

    pfs.remove(CONTROL_GROUP_ID)
    DATA_GROUP_ID = list(pfs)[0]

    # collect positions of column names for both groups
    firstGroupNames = []
    secondGroupNames = []
    for name in names:
        if name.startswith(CONTROL_GROUP_ID):
            firstGroupNames.append(name)
        elif name.startswith(DATA_GROUP_ID):
            secondGroupNames.append(name)
    #end

    controlGroupNames = firstGroupNames
    dataGroupNames = secondGroupNames

    # collect positions of column names for both groups
    controlGroupNames = dict.fromkeys(controlGroupNames)
    dataGroupNames = dict.fromkeys(dataGroupNames)
    for name in controlGroupNames:
        controlGroupNames[name] = names.index(name)
    for name in dataGroupNames:
        dataGroupNames[name] = names.index(name)


    # parse and store the actual data

    # read values
    data = {}
    ndup = 0
    ln = 0
    #refresh = (len(self.lines)-1) / 10
    #self.progressBar = ProgressBar(self, iterations=25)
    for elts in lines[1:]:
        ln += 1
        #if ln%refresh == 0:
            #self.progressBar.advance()

        if len(elts) != len(names)+1: # EntrezID is the first value
            raise ValueError('Wrong number of values, line: %d' % ln)
        try:
            geneID = str(elts[0])
            vals = [float(x) for x in elts[1:]]
        except Exception, e:
            raise ValueError('Error while reading values, line: %d' % ln)
        else:
            if data.has_key(geneID):
                ndup += 1
            else:
                # init storage
                data[geneID] = {}
                data[geneID][CONTROL_GROUP_KEY] = {}
                data[geneID][DATA_GROUP_KEY] = {}

                for atrName in controlGroupNames.keys():
                    data[geneID][CONTROL_GROUP_KEY][atrName] = []

                for atrName in dataGroupNames.keys():
                    data[geneID][DATA_GROUP_KEY][atrName] = []

            # get values for first group of columns
            for (name, index) in controlGroupNames.items():
                data[geneID][CONTROL_GROUP_KEY][name].append(vals[index])

            # get values for second group of columns
            for (name, index) in dataGroupNames.items():
                data[geneID][DATA_GROUP_KEY][name].append(vals[index])
            #end else
    #endfor


    ## merge duplicates by averaging
    for geneID in data.keys():
        for atrName in data[geneID][CONTROL_GROUP_KEY].keys():
            values = data[geneID][CONTROL_GROUP_KEY][atrName]
            data[geneID][CONTROL_GROUP_KEY][atrName] = sum(values) / float(len(values))

        for atrName in data[geneID][DATA_GROUP_KEY].keys():
            values = data[geneID][DATA_GROUP_KEY][atrName]
            data[geneID][DATA_GROUP_KEY][atrName] = sum(values) / float(len(values))



    ## merge duplicates by averaging
    #if self.ui.meanRadioButton.isChecked():
        #for geneID in data.keys():
            #for atrName in data[geneID][CONTROL_GROUP_KEY].keys():
                #values = data[geneID][CONTROL_GROUP_KEY][atrName]
                #data[geneID][CONTROL_GROUP_KEY][atrName] = sum(values) / float(len(values))

            #for atrName in data[geneID][DATA_GROUP_KEY].keys():
                #values = data[geneID][DATA_GROUP_KEY][atrName]
                #data[geneID][DATA_GROUP_KEY][atrName] = sum(values) / float(len(values))

    ## merge duplicates by median
    #elif self.ui.medianRadioButton.isChecked():
        #for geneID in data.keys():
            #for atrName in data[geneID][CONTROL_GROUP_KEY].keys():
                #values = data[geneID][CONTROL_GROUP_KEY][atrName]
                #data[geneID][CONTROL_GROUP_KEY][atrName] = median(values)

            #for atrName in data[geneID][DATA_GROUP_KEY].keys():
                #values = data[geneID][DATA_GROUP_KEY][atrName]
                #data[geneID][DATA_GROUP_KEY][atrName] = median(values)

    ## take one duplicate at random
    #elif self.ui.randomRadioButton.isChecked():
        #for geneID in data.keys():
            #for atrName in data[geneID][CONTROL_GROUP_KEY].keys():
                #values = data[geneID][CONTROL_GROUP_KEY][atrName]
                #data[geneID][CONTROL_GROUP_KEY][atrName] = choice(values)

            #for atrName in data[geneID][DATA_GROUP_KEY].keys():
                #values = data[geneID][DATA_GROUP_KEY][atrName]
                #data[geneID][DATA_GROUP_KEY][atrName] = choice(values)
    ##end

    namesDict = {CONTROL_GROUP_KEY: controlGroupNames, DATA_GROUP_KEY: dataGroupNames}
    table = __makeExampleTable(namesDict, data)

    logFCs = {}
    if calcMethod == 'ratio':
        if dataFormat == 'log2':  # log2 data have to be transformed for ratio computation
            for geneID in data.keys():
                for attrName in namesDict[CONTROL_GROUP_KEY]:
                    data[geneID][CONTROL_GROUP_KEY][attrName] = math.pow(2, data[geneID][CONTROL_GROUP_KEY][attrName])
                for attrName in namesDict[DATA_GROUP_KEY]:
                    data[geneID][DATA_GROUP_KEY][attrName] = math.pow(2, data[geneID][DATA_GROUP_KEY][attrName])

        for geneID in data.keys():
            control_array = [data[geneID][CONTROL_GROUP_KEY][attrName] for attrName in namesDict[CONTROL_GROUP_KEY]]
            data_array = [data[geneID][DATA_GROUP_KEY][attrName] for attrName in namesDict[DATA_GROUP_KEY]]

            numerator = mean(data_array)
            denumerator = mean(control_array)

            if numerator < 0 or denumerator < 0:
                print 'Invalid values, gene %s' % str(geneID)
                continue

            logFCs[geneID] = numerator / denumerator
            # for those less than 1 invert and give negative sign
            if logFCs[geneID] < 1:
                logFCs[geneID] = -1.0 / logFCs[geneID]
    else:
        # difference
        if dataFormat == 'linear':  # linear data have to be transformed for log2 difference computation
            for geneID in data.keys():
                for attrName in namesDict[CONTROL_GROUP_KEY]:
                    if data[geneID][CONTROL_GROUP_KEY][attrName] <= 0:
                        raise ValueError('Cannot transform linear data to log2: value is <= 0 for gene %s' % str(geneID))
                    else:
                        data[geneID][CONTROL_GROUP_KEY][attrName] = math.log(data[geneID][CONTROL_GROUP_KEY][attrName], 2)
                for attrName in namesDict[DATA_GROUP_KEY]:
                    if data[geneID][DATA_GROUP_KEY][attrName] <= 0:
                        raise ValueError('Cannot transform linear data to log2: value is <= 0 for gene %s' % str(geneID))
                    else:
                        data[geneID][DATA_GROUP_KEY][attrName] = math.log(data[geneID][DATA_GROUP_KEY][attrName], 2)

        for geneID in data.keys():
            control_array = [data[geneID][CONTROL_GROUP_KEY][attrName] for attrName in namesDict[CONTROL_GROUP_KEY]]
            data_array = [data[geneID][DATA_GROUP_KEY][attrName] for attrName in namesDict[DATA_GROUP_KEY]]
            logFCs[geneID] = mean(data_array) - mean(control_array)
    #end

    # print dataGroupNames
    # print controlGroupNames
    sortedLogFCs = [(elt[1], elt[0]) for elt in sorted([(logFCs[geneID], geneID) for geneID in logFCs.keys()], reverse=True)] #data.keys()], reverse=True)]

    return {'table': table, 'fold_change': sortedLogFCs}
#end


# this function creates a table from SEGS rules where columns are terms
def __make_rule_term_example_table(tableDict, allTerms):
    import orange
    import constants as const

    attrList = [orange.EnumVariable(name=str(term), values=[const.PRESENT, const.ABSENT]) for term in allTerms]

    # three meta attributes
    ruleName = orange.StringVariable(const.NAME_ATTR)
    mid = orange.newmetaid()
    ruleTerms = orange.StringVariable(const.TERMS_ATTR)
    mid1 = orange.newmetaid()
    #ruleNumber = orange.EnumVariable(SEQ_NUM_ATTR) #StringVariable(SEQ_NUM_ATTR)
    ruleNumber = orange.FloatVariable(const.SEQ_NUM_ATTR, startValue=1, endValue=len(tableDict), stepValue=1, numberOfDecimals=0)
    mid2 = orange.newmetaid()


    # this is a classless domain
    domain = orange.Domain(attrList, False)

    # name of the rule is a meta attribute
    domain.addmeta(mid, ruleName, False)
    domain.addmeta(mid1, ruleTerms, False)
    domain.addmeta(mid2, ruleNumber, False)

    table = orange.ExampleTable(domain)

    for k in sorted(tableDict.keys()):
        exampleValues = []
        for (i,term) in enumerate(allTerms):
            if term in tableDict[k][const.RULETERMS_KEY]:
                #exampleValues.append(PRESENT)
                exampleValues.append(orange.Value(attrList[i], const.PRESENT))
            else:
                #exampleValues.append(ABSENT)
                exampleValues.append(orange.Value(attrList[i], const.ABSENT))
        example = orange.Example(domain, exampleValues)
        #example[NAME_ATTR] = tableDict[k][RULENAME_KEY][1:-1]    #skip square brackets from the string
        #example[TERMS_ATTR] = tableDict[k][RULETERMS_STR_KEY][1:-1]
        #example[SEQ_NUM_ATTR] = k

        example[const.NAME_ATTR] = orange.Value(ruleName, tableDict[k][const.RULENAME_KEY][1:-1])    #skip square brackets from the string
        example[const.TERMS_ATTR] = orange.Value(ruleTerms, tableDict[k][const.RULETERMS_STR_KEY][1:-1])
        example[const.SEQ_NUM_ATTR] = orange.Value(ruleNumber, k)

        table.append(example)
    #end
    return table
#end



# this function creates a table from SEGS rules where columns are genes
def __make_rule_gene_example_table(tableDict, genes):
    import orange
    import constants as const
    # attributes are rules (all conjuncts of a rule form the name of the attribute)
    #attrList = [orange.EnumVariable(name=ruleString[1:-1].replace(' ', '_'), values=[PRESENT, ABSENT])
    #            for ruleString in tableDict.keys()]

    attrList = [orange.EnumVariable(name=str(gene), values=[const.PRESENT, const.ABSENT]) for gene in genes]

    # three meta attributes
    ruleName = orange.StringVariable(const.NAME_ATTR)
    mid = orange.newmetaid()
    ruleTerms = orange.StringVariable(const.TERMS_ATTR)
    mid1 = orange.newmetaid()
    #ruleNumber = orange.EnumVariable(SEQ_NUM_ATTR) #StringVariable(SEQ_NUM_ATTR)
    ruleNumber = orange.FloatVariable(const.SEQ_NUM_ATTR, startValue=1, endValue=len(tableDict), stepValue=1, numberOfDecimals=0)
    mid2 = orange.newmetaid()


    # this is a classless domain
    domain = orange.Domain(attrList, False)

    # name of the rule is a meta attribute
    domain.addmeta(mid, ruleName, False)
    domain.addmeta(mid1, ruleTerms, False)
    domain.addmeta(mid2, ruleNumber, False)

    table = orange.ExampleTable(domain)

    for k in sorted(tableDict.keys()):
        exampleValues = []
        for (i,gene) in enumerate(genes):
            #if gene in tableDict[k][GENES_KEY]:
            if gene in tableDict[k][const.TOP_GENES_KEY]:
                #exampleValues.append(PRESENT)
                exampleValues.append(orange.Value(attrList[i], const.PRESENT))
            else:
                exampleValues.append(orange.Value(attrList[i], const.ABSENT))
                #exampleValues.append(ABSENT)
        example = orange.Example(domain, exampleValues)
        example[const.NAME_ATTR] = tableDict[k][const.RULENAME_KEY][1:-1]    #skip square brackets from the string
        example[const.TERMS_ATTR] = tableDict[k][const.RULETERMS_STR_KEY][1:-1]
        example[const.SEQ_NUM_ATTR] = k

        example[const.NAME_ATTR] = orange.Value(ruleName, tableDict[k][const.RULENAME_KEY][1:-1])    #skip square brackets from the string
        example[const.TERMS_ATTR] = orange.Value(ruleTerms, tableDict[k][const.RULETERMS_STR_KEY][1:-1])
        example[const.SEQ_NUM_ATTR] = orange.Value(ruleNumber, k)

        table.append(example)
    #end
    return table
#end


def segmine_rules_as_table(input_dict):
    import constants as const

    rules = input_dict['rules']
    tableDict = {}
    allGenes = set()
    allGenesDE = set()
    allTerms = set()

    for (i, rule) in enumerate(rules):
        # beware, there can be also rules with only interacting terms...wtf...
        if const.RULETERMS_STR_KEY in rule[const.DESCRIPTION_KEY]:
            TERMids = [x[const.TERMID_KEY] for x in rule[const.DESCRIPTION_KEY][const.RULETERMS_STR_KEY]]
            TERMnames = [x[const.TERMNAME_KEY] for x in rule[const.DESCRIPTION_KEY][const.RULETERMS_STR_KEY]]
        else:
            TERMids = []
            TERMnames = []
        INTids = []
        INTnames = []
        if const.INTTERMS_KEY in rule[const.DESCRIPTION_KEY]:
            INTids = [x[const.TERMID_KEY] for x in rule[const.DESCRIPTION_KEY][const.INTTERMS_KEY]]
            INTnames = [x[const.TERMNAME_KEY] for x in rule[const.DESCRIPTION_KEY][const.INTTERMS_KEY]]

        ruleTerms = TERMids + INTids
        ruleTermNames = TERMnames + INTnames
        ruleGenes = rule[const.COVGENES_KEY]
        ruleGenesDE = rule[const.COVTOPGENES_KEY]

        tableDict[i] = {}
        tableDict[i][const.GENES_KEY] = dict.fromkeys(ruleGenes)
        tableDict[i][const.TOP_GENES_KEY] = dict.fromkeys(ruleGenesDE)
        tableDict[i][const.RULENAME_KEY] = str(ruleTermNames)
        tableDict[i][const.RULETERMS_STR_KEY] = str(ruleTerms)
        tableDict[i][const.RULETERMS_KEY] = ruleTerms
        allGenes.update(ruleGenes)
        allGenesDE.update(ruleGenesDE)
        allTerms.update(ruleTerms)
    #endfor

    geneTable = __make_rule_gene_example_table(tableDict, sorted(list(allGenesDE)))
    termTable = __make_rule_term_example_table(tableDict, sorted(list(allTerms)))
    return {'gene_table': geneTable, 'term_table': termTable}
#end


def filter_unknown_genes_stu(input_dict):
    import cPickle
    from os.path import normpath, join, dirname

    ranks = input_dict['gene_ranks']
    genes = cPickle.load(open(normpath(join(dirname(__file__), 'data/genes_stu.pickle')), 'rb'))

    result = []
    unknown = 0
    for gene, rank in ranks:
        gene = gene.lower()
        if gene in genes:
            result.append((gene, rank))
        else:
            unknown += 1
    if unknown:
        logging.warning('There were %d unknown STU genes.' % unknown)

    return {'filtered_ranks': result}
#end


def filter_unknown_genes_ath(input_dict):
    import cPickle
    from os.path import normpath, join, dirname

    ranks = input_dict['gene_ranks']
    genes = cPickle.load(open(normpath(join(dirname(__file__), 'data/genes_ath.pickle')), 'rb'))

    result = []
    unknown = 0
    for gene, rank in ranks:
        gene = gene.lower()
        if gene in genes:
            result.append((gene, rank))
        else:
            unknown += 1
    if unknown:
        logging.warning('There were %d unknown ATH genes.' % unknown)

    return {'filtered_ranks': result}
#end


def segmine_cutoff(input_dict):
    from numpy import array

    ub = float(input_dict['upper']) if input_dict['upper'].strip() else None
    lb = float(input_dict['lower']) if input_dict['lower'].strip() else None
    ranks = input_dict['ranks']
    logfcs = input_dict['logfcs']
    takeAbs = True if input_dict['absolute'].lower() == 'true' else False

    if ub and lb and ub <= lb:
        raise Exception('Invalid bounds')

    ranksDict = {}
    for pair in ranks:
        ranksDict[pair[0]] = pair[1]

    logfcsarr = array([float(elt[1]) for elt in logfcs])
    if takeAbs:
        logfcsarr = abs(logfcsarr)
    UB = max(logfcsarr) if ub == None  else  ub
    LB = min(logfcsarr) if lb == None  else  lb

    resultRanks = []
    resultLogFCs = []
    errors = []
    for (ID, value) in logfcs:
        tmp = abs(value) if takeAbs  else  value
        if tmp <= UB and tmp >= LB:
            if ID not in ranksDict:
                errors.append(ID)
            else:
                resultRanks.append((ranksDict[ID], ID))
                resultLogFCs.append((value, ID))
    # end

    resultRanks.sort(reverse=True)
    resultRanks = [(elt[1], elt[0]) for elt in resultRanks]
    resultLogFCs.sort(reverse=True)
    resultLogFCs = [(elt[1], elt[0]) for elt in resultLogFCs]
    if errors:
        logging.warning('%d genes ignored because ranks were not present' % len(errors))

    return {'filtered_ranks': resultRanks, 'filtered_logfcs': resultLogFCs}


def resolve_gene_names_STU(input_dict):
    import cPickle
    from os.path import normpath, join, dirname

    ranks = input_dict['gene_ranks']
    mapping = cPickle.load(open(normpath(join(dirname(__file__), 'data/probe2rep_STU.pickle')), 'rb'))

    result = []
    unknown = 0
    for (gene, rank) in ranks:
        if gene in mapping:
            result.append((mapping[gene], rank))
        else:
            #result.append((gene, rank))
            unknown += 1
    if unknown:
        logging.warning('There were %d unknown STU probe names.' % unknown)

    # remove duplicates and sort again
    result = list(set(result))
    result = [(x[1], x[0]) for x in sorted([(x[1], x[0]) for x in result], reverse=True)]
    return {'mapped_ranks': result}


def segmine_do_hclustering(input_dict):
    import Orange

    table = input_dict['table']
    linkage = int(input_dict['linkage'])
    metric = int(input_dict['metric'])
    linkages = {1: Orange.clustering.hierarchical.SINGLE,
                2: Orange.clustering.hierarchical.AVERAGE,
                3: Orange.clustering.hierarchical.COMPLETE,
                4: Orange.clustering.hierarchical.WARD}
    dmetrices = {1: Orange.distance.Euclidean,
                 2: Orange.distance.Hamming,
                 3: Orange.distance.Maximal,
                 4: Orange.distance.Manhattan,
                 5: Orange.distance.Relief,
                 6: Orange.distance.PearsonR,
                 7: Orange.distance.SpearmanR,
                 8: Orange.distance.Mahalanobis}

    dmatrix = Orange.distance.distance_matrix(table, dmetrices[metric])
    clustering = Orange.clustering.hierarchical.HierarchicalClustering()
    clustering.linkage = linkages[linkage]
    hcl = clustering(dmatrix)
    hcl.mapping.objects = table
    return {'hcluster': hcl}


def segmine_hclustering(input_dict):
    return {'selected_examples': None}


def segmine_hclustering_finished(postdata, input_dict, output_dict):
    import Orange

    def assign_numbers(root):
        nodes = Orange.clustering.hierarchical.preorder(root)
        for (i, node) in enumerate(nodes):
            node.setattr('idx', i)

    try:
        cid = int(postdata['selected_cluster'][0])
    except ValueError:
        raise SyntaxError("Invalid cluster number! Please enter a valid number.")
    widget_pk = postdata['widget_id'][0]
    cluster = input_dict['hclustering']

    assign_numbers(cluster)
    clusters = Orange.clustering.hierarchical.cluster_to_list(cluster)
    selected = [x for x in clusters if x.idx == cid][0]
    table = Orange.data.Table(selected[0].domain)
    table.extend([x for x in selected])
    return {'selected_examples': table}


def segmine_ruletable2attribute_union_intersection(input_dict):
    import Orange
    import constants
    table = input_dict['ruletable']

    def allGenes(names):
        for name in names:
            try:
                gid = int(name)
            except ValueError:
                return False
        return True
    #end

    def allOntology(names):
        for name in names:
            if not name.startswith(constants.GO_PREFIX) and not name.startswith(constants.KEGG_PREFIX) and not \
                    name.startswith(constants.GOMAPMAN_PREFIX):
                return False
        return True
    #end

    names = [x.name for x in table.domain.attributes]
    allgenes = allGenes(names)
    allgokegg = allOntology(names)
    if not allgenes and not allgokegg:
        raise ValueError('This widget only accepts SegMine rule table.')

    union = names
    intersection = []
    for name in names:
        if all([bool(int(example[name])) for example in table]):
            intersection.append(name)

    if allgenes:
        union = [constants.ENTREZ_GENE_PREFIX + ':' + x for x in union]
        intersection = [constants.ENTREZ_GENE_PREFIX + ':' + x for x in intersection]

    return {'atrUnion': union, 'atrInter': intersection}


def segmine_biomine_search_plants(input_dict):
    import urllib
    import urllib2
    import json
    url = 'http://biomine.ijs.si/api'

    qterms = input_dict['qterms']
    maxnodes = input_dict['maxnodes'].strip()
    dbname = input_dict['dbname'].strip()

    qdict = {'query': ' '.join(qterms)}
    if maxnodes:
        qdict['maxnodes'] = int(maxnodes)
    if dbname:
        qdict['database'] = str(dbname)

    params = urllib.urlencode(qdict)
    result = json.loads(urllib2.urlopen(url, params).read())
    if 'error' in result:
        raise Exception(result['error'])

    return {'bmgraph': result['graph']}