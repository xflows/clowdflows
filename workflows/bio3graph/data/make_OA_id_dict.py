import urllib2
import cPickle
from os.path import join, abspath, normpath, dirname

FILE_LIST_URL = 'ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/file_list.txt'

dname = dirname(__file__)
mapping = {}

fp = urllib2.urlopen(FILE_LIST_URL)
lines = fp.readlines()
for line in lines:
    elts = line.split('\t')
    if len(elts) != 3:
        continue
    pmid = elts[2].strip().replace('PMC', '')
    mapping[pmid] = None

fp = open(normpath(join(dname, 'OA_dict.pickle')), 'w')
cPickle.dump(mapping, fp, cPickle.HIGHEST_PROTOCOL)
fp.close()

