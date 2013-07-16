import cPickle
import pickle
import cStringIO
import StringIO
import gzip
import urllib

# NCBI data ftp.
NCBI_URL = 'http://mirrors.vbi.vt.edu/mirrors/ftp.ncbi.nih.gov/gene/DATA/'
NCBI_ORGANISM_GENE_DATA = 'GENE_INFO/Mammalia/'

# for gene_info file
FORMAT_LINE_START = '#Format:'
ENTREZ_ID = 'GeneID'
GENE_SYMBOL = 'Symbol'
SYNONYMS =  'Synonyms'
NEWENTRY = 'NEWENTRY'
ENTREZ_ID_COLNO = 1
GENE_SYMBOL_COLNO = 2
SYNONYMS_COLNO = 4

# for gene2accession file
tax_id = 0
GeneID = 1
status = 2
RNA_nucleotide_accession = 3
RNA_nucleotide_gi = 4
protein_accession = 5
protein_gi = 6
genomic_nucleotide_accession = 7


EMPTY = '-'



class Mappings(object):
    def __init__(self, entrez2symbol={}, symbol2entrez={}, entrez2synonyms={}, synonyms2entrez={}, errors=[]):
        self.entrez2symbol = entrez2symbol
        self.symbol2entrez = symbol2entrez
        self.entrez2synonyms = entrez2synonyms
        self.synonyms2entrez = synonyms2entrez

    #end
#end class


def getMapping():

    entrez2symbol = {}
    symbol2entrez = {}
    entrez2synonyms = {}
    synonyms2entrez = {}

    for fname in ['Homo_sapiens.gene_info.gz']: #, 'Mus_musculus.gene_info.gz', 'Rattus_norvegicus.gene_info.gz']:
    #for fname in ['gene_info_small']:
        #fp = open(fname, 'r')

        # Download gzip file.
        web_fp = urllib.urlopen('%s%s%s' % (NCBI_URL, NCBI_ORGANISM_GENE_DATA, fname))
        fp = gzip.GzipFile(fileobj=StringIO.StringIO(web_fp.read()))

        print 'Reading file "%s"' % fname
        ln = 0
        while True:
            ln += 1
            if ln%100000 == 0:
                print 'at line %d' % ln

            line = fp.readline().strip()
            if not line: #EOF
                break

            elts = line.split('\t')
            if elts[0].startswith('#'):
                continue

            ## if there is header, check it
            #if elts[0] ==  FORMAT_LINE_START:
                #if len(elts) < 3 or elts[ENTREZ_ID_COLNO+1] != ENTREZ_ID or elts[GENE_SYMBOL_COLNO+1] != GENE_SYMBOL or \
                   #elts[SYNONYMS_COLNO+1] != SYNONYMS:
                    #raise TypeError('Input file is not an Entrez gene info file.')
                #continue

            entid = int(elts[ENTREZ_ID_COLNO])
            if entid in entrez2symbol or entid in entrez2synonyms:
                print 'duplicate GeneID (Entrez ID): %d' % entid
                continue

            # get gene symbol
            symbol = elts[GENE_SYMBOL_COLNO].lower()
            if symbol == NEWENTRY:
                continue
            # we ignore symbols which are not unique
            if symbol in symbol2entrez:
                print 'Not unique: ', symbol, entid
                eid = symbol2entrez[symbol]
                del entrez2symbol[eid]
                del symbol2entrez[symbol]
                continue
            entrez2symbol[entid] = symbol
            symbol2entrez[symbol] = entid

            # get gene synonyms
            syns = [s.strip().lower() for s in elts[SYNONYMS_COLNO].split('|')]
            syns = [s for s in syns if s!=EMPTY]
            # we ignore synonyns which are not unique
            entrez2synonyms[entid] = []
            for s in syns:
                if s in synonyms2entrez:
                    e = synonyms2entrez[s]
                    entrez2synonyms[e].remove(s)
                    del synonyms2entrez[s]
                else:
                    synonyms2entrez[s] = entid
                    entrez2synonyms[entid].append(s)
            #end

        #end while

        fp.close()
        print 'Done.'

    #end for
    print len(synonyms2entrez), len(entrez2synonyms)

    return Mappings(entrez2symbol=entrez2symbol, symbol2entrez=symbol2entrez, entrez2synonyms=entrez2synonyms,\
                    synonyms2entrez=synonyms2entrez)
#end


def mappingModuleAll(a):
    #a = cPickle.load(open('mappingsHMR.pickle'))
    assert(isinstance(a, Mappings))

    ofp = open('mappings/mappingsFull.py', 'w')
    ofp.write('from pickle import load\n')
    ofp.write('from cStringIO import StringIO\n\n')

    for attr in ['symbol2entrez', 'synonyms2entrez', 'entrez2symbol', 'entrez2synonyms']:
        outS = cStringIO.StringIO()
        d = getattr(a, attr)
        pickle.dump(d, outS)
        outS.flush()
        #ofp.write("%s = load(StringIO('''%s'''))\n" % (attr, b64encode(outS.getvalue())))
        ofp.write("%s = load(StringIO('''%s'''))\n" % (attr, outS.getvalue()))
    ofp.close()
#end

mapp = getMapping()
fp = open('entrez2symbol.pickle', 'wb')
cPickle.dump(mapp.entrez2symbol, fp, cPickle.HIGHEST_PROTOCOL)
