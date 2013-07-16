import urllib
import urllib2
import csv
import cStringIO
import os
import cPickle
import time
from os.path import join, normpath, dirname

# http://gpsdb.expasy.org/cgi-bin/gpsdb/show?name=interleukin%208&model=Homo%20sapiens&type=gene&format=txt
# gpsdb.expasy.org/cgi-bin/gpsdb/show?name=indy&format=txt


#http://gpsdb.expasy.org/cgi-bin/gpsdb/show?model=Homo+sapiens&type=gene&name=interleukin+8&format=txt

GPSDB_URL = 'http://gpsdb.expasy.org/cgi-bin/gpsdb/show?'
NAME = 'name'
MODEL = 'model'
TYPE = 'type'
FORMAT = 'format'

MODEL_HUMAN = 'Homo sapiens'
TYPE_GENE = 'gene'
TXT_FORMAT = 'txt'

#entrez2symbol = cPickle.load(open('mappings/e2symb'))
#entrez2synonyms = cPickle.load(open('mappings/e2syns'))


class Synonym_extractor(object):
    qwait = 0.5
    cache_fname = 'data/gene_synonyms_cache'

    def __init__(self):
        self.last_qdate = 0
        try:
            self._cache = cPickle.load(open(normpath(join(dirname(__file__), self.cache_fname))))
        except Exception:
            self._cache = {}
    #end

    def _flush_cache(self):
        #fp = open(self.cache_fname, 'wb')
        fp = open(normpath(join(dirname(__file__), self.cache_fname)), 'wb')
        cPickle.dump(self._cache, fp)
        fp.close()
    #end

    def __del__(self):
        self._flush_cache()

    def wait(self):
        # 2 requests per second (for now)
        td = time.time() - self.last_qdate
        if td < self.qwait:
            print 'sleeping for %.2f seconds' % (self.qwait - td)
            time.sleep(self.qwait - td)
    #end

    def make_query_url(self, geneSymbol):
        params = {NAME: geneSymbol, MODEL: MODEL_HUMAN, TYPE: TYPE_GENE, FORMAT: TXT_FORMAT}
        qstring = GPSDB_URL + urllib.urlencode(params)
        return qstring
    #end

    def get_gene_synonyms(self, geneSymbol):
        if geneSymbol in self._cache:
            return self._cache[geneSymbol]

        self.wait()
        qstring = self.make_query_url(geneSymbol)
        try:
            fs = cStringIO.StringIO(urllib2.urlopen(urllib2.Request(qstring)).read())
            #print fs.getvalue()
            table = csv.reader(fs, delimiter='\t')
        except Exception, e:
            print e.message
            return []

        synonyms = [row[0] for row in table]
        synonyms = [x.lower() for x in synonyms]
        synonyms = list(set(synonyms))

        self._cache.update({geneSymbol: synonyms})
        return synonyms
    #end

    def get_geneset_synonyms(self, genes):
        assert(isinstance(genes, list))
        result = {}
        for gene in genes:
            result[gene] = self.get_gene_synonyms(gene)

        return result
    #end

#end class



#res['A']['gsea', 'fisher', 'all', 'page'][1, 2, 3,...]['topGenes', 'terms', 'allGenes', 'scores']
# ['z_score', 'page_p', 'gsea_p', 'enrichment_score', 'unadjusted_p', 'fisher_p', 'aggregate_p']

# def get_synonyms_for_results(resultsFile):
#     assert(os.path.isfile(resultsFile))
#
#     res = cPickle.load(open(resultsFile))
#     geneSets = {}
#     for ruleNum in res['A']['all'].keys():
#         rule = res['A']['all'][ruleNum]
#         geneSets[ruleNum] = rule['topGenes']
#
#
#     synonyms = {}
#     se = Synonym_extractor()
#     for ruleNum in geneSets.keys():
#         synonyms[ruleNum] = se.get_geneset_synonyms(geneSets[ruleNum])
#
#         fp = open('result_genes2syns.pickle', 'wb')
#         cPickle.dump(synonyms, fp, cPickle.HIGHEST_PROTOCOL)
#         fp.close()
#         print 'written rule %d, genes: %d' % (ruleNum, len(synonyms[ruleNum]))
#     return synonyms
# #end
#
#
# def synMapping():
#     res_syns = cPickle.load(open('data/result_genes2syns.pickle'))
#     syn_map = {}
#     for ruleNum in res_syns.keys():
#         for egid in res_syns[ruleNum].keys():
#             if egid not in syn_map:
#                 syn_map[egid] = res_syns[ruleNum][egid]
#     return syn_map
#
# #a = synMapping()
# #fp = open('data/gene2syn', 'wb')
# #cPickle.dump(a, fp, cPickle.HIGHEST_PROTOCOL)
# #fp.close()


#e2syns = cPickle.load(open('mappings/e2syns'))
#result_syns = cPickle.load(open('data/result_genes2syns.pickle'))
#for ruleNum in result_syns.keys():
    #for egid in result_syns[ruleNum].keys():
        #gpsdb = result_syns[ruleNum][egid]
        #ncbi = list(set(e2syns[egid]))
        #joined = list(set(gpsdb).union(set(ncbi)))
        #diff = set(ncbi).difference(gpsdb)
        ##print ncbi
        #if diff:
            #print diff
            #print 'gpsdb: %d, ncbi: %d, total: %d' %(len(gpsdb), len(ncbi), len(joined))
############


# a = Synonym_extractor()
# s = a.get_geneset_synonyms(['lgals3', 'sfpq', 'ddx39b', 'srsf11', 'cir1', 'luc7l3', 'prpf39', 'hnrnph1', 'lgals3', 'sfpq', 'ddx39b'])
##a = get_synonyms_for_results('data/ALL-FINAL.pickle')

