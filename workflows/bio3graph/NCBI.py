import urllib2
import urllib
import os
from xml.etree import cElementTree as tree
import time
import pickle
import re
import xml.dom.minidom as dom
import cPickle
import csv
import codecs
import unidecode


class Document(object):
    def __init__(self):
        self.docid = None
        self.year = None
        self.title = None
        self.abstract = None
        self.body = None
        #self.text = None
        self.xml = None
    #end

    def write_content_text(self, outdir, utf=True):
        assert(os.path.isdir(outdir))
        if utf:
            fp = codecs.open(os.path.join(outdir, self.docid + '.txt'), 'w', encoding='utf-8')
            fp.write(self.title + '\n' + self.abstract + '\n' + self.body)
        else:
            fp = open(os.path.join(outdir, self.docid + '.txt'), 'w')
            fp.write(unidecode.unidecode(self.title) + '\n' + unidecode.unidecode(self.abstract) + '\n' + unidecode.unidecode(self.body))
        fp.close()
#end


class NCBI_Extractor(object):
    #qwait = 0.33
    qwait = 1.0
    maxdoc = 1000

    searchURL = 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi'
    fetchURL = 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi'
    pmcURL = 'http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3315246/'

    def __init__(self):
        self.last_qdate = 0
    #end

    def dispatchRequest(self, q):
        # obey NCBI limitations (3 requests per second)
        td = time.time() - self.last_qdate
        if td < self.qwait:
            print 'sleeping for %.2f seconds' % (self.qwait - td)
            time.sleep(self.qwait - td)

        self.last_qdate = time.time()
        return urllib2.urlopen(urllib2.Request(q)).read()
    #end

    def getIDs(self, queryURL, maxHits=0):
        ids = []
        cnt = 1

        # first batch of results
        result = self.dispatchRequest(queryURL)
        t = tree.fromstring(result)
        ids.extend([x.text for x in t.find('IdList').findall('Id')])
        hits = int(t.find('Count').text)
        print 'Total hits: ', hits

        print 'batch: %d, got: %d' % (cnt, len(ids))

        # if we have enough already
        if maxHits > 0 and (len(ids) > maxHits or maxHits > hits):
            return ids[:maxHits]

        # if there are more, get them also with retstart option
        while len(ids) < hits:
            nq = queryURL + '&retstart=%d&retmax=%d' % (len(ids), self.maxdoc)
            result = self.dispatchRequest(nq)
            t = tree.fromstring(result)
            ids.extend([x.text for x in t.find('IdList').findall('Id')])
            cnt += 1
            print 'batch: %d, total: %d' % (cnt, len(ids))
            if maxHits and len(ids) >= maxHits:
                break
        #end
        if maxHits:
            return ids[:maxHits]
        else:
            return ids
    #end

    def query(self, queryText, db='pmc', maxHits=0):
        if not queryText:
            raise ValueError('Empty query!')

        query = [('db', db), ('term', queryText)]

        query.append(('retmax', self.maxdoc))
        query = '%s?%s' % (self.searchURL, urllib.urlencode(query))
        ids = self.getIDs(query, maxHits=maxHits)
        return ids

    #end

    def getDocument(self, did, db='pmc'):
        xml = self.getXML(did, db)
        root = dom.parseString(xml)
        doc = self.extractArticleText(root, did)
        doc.docid = did
        doc.xml = xml
        return doc
    #end

    def getXML(self, did, db='pmc'):
        query = [('db', db), ('id', did)]
        url = '%s?%s' % (self.fetchURL, urllib.urlencode(query))
        xml = self.dispatchRequest(url)
        return xml
    #end

    def getFulltext(self, did):
        xml = self.getXML(did)
        root = dom.parseString(xml)
        doc = self.extractArticleText(root, did)
        return doc

    def getDocumentFromXMLfile(self, fname, did=None):
        #xml = codecs.open(fname, encoding='utf-8').read()
        if not did:
            did = os.path.splitext(os.path.split(fname)[1])[0]
        xml = open(fname).read()
        root = dom.parseString(xml)
        doc = self.extractArticleText(root, did)
        doc.docid = did
        doc.xml = xml
        return doc
    #end

    def extractArticleText(self, root, did,only_sections=None):
        try:
            titleNode = root.getElementsByTagName('article-title')[0]
        except Exception:
            title = ''
            print 'Warning: no title found, document %s' % str(did)
        else:
            title = self.list2text(self.recursiveCollect(titleNode, []))

        try:
            abstractNode = root.getElementsByTagName('abstract')[0]
        except Exception:
            abstract = ''
            print 'Warning: no abstract found, document %s' % str(did)
        else:
            abstract = self.list2text(self.recursiveCollect(abstractNode, []))
            abstract = re.sub('(\[)[ ,-:;]*(\])', '', abstract) # remove what remains of citations

        try:
            bodyNode = root.getElementsByTagName('body')[0]
        except Exception:
            body = ''
            print 'Warning: no body found, document %s' % str(did)
        else:
            body = self.list2text(self.recursiveCollect(bodyNode, [],only_sections))
            body = re.sub('(\[)[ ,-:;]*(\])', '', body)

        ytags = root.getElementsByTagName('pub-date')
        years = []
        for x in ytags:
            y = x.getElementsByTagName('year')
            if y:
                years.append(int(y[0].childNodes[0].data))
        year = min(years)

        new = Document()
        new.year = year
        new.title = title
        new.abstract = abstract
        new.body = body
        #new.text = abstract + ' ' + body
        return new
    #end

    #
    def recursiveCollect(self, node, result, only_sections=None,skipTags=['title', 'xref', 'table', 'graphic', 'ext-link',
                                                       'media', 'inline-formula', 'disp-formula']):
        for child in node.childNodes:
            if child.nodeType == dom.Node.ELEMENT_NODE:
                if child.tagName not in skipTags:
                    print child.getAttribute("sec-type"), child.tagName
                    if not only_sections or child.tagName!='sec' or not child.hasAttribute('sec-type') or child.getAttribute('sec-type') in only_sections:
                        self.recursiveCollect(child, result,only_sections)
            elif child.nodeType == dom.Node.TEXT_NODE:
                result.append(child.data)
        #endfor

        return result
    #end

    def recursiveCollectET(self, node, result, only_sections=None,skipTags=['title', 'xref', 'table', 'graphic', 'ext-link',
                                                       'media', 'inline-formula', 'disp-formula']):
        if node.tag not in skipTags:
            #print node.attrib["sec-type"], node.tagName
            if not only_sections or node.tag!='sec' or not node.attrib['sec-type'] or node.attrib['sec-type'] in only_sections:
                if node.text and node.text.strip()!="":
                    result.append(node.text.replace('\n', ''))
                if node.tail and node.tail.strip()!="":
                    result.append(node.tail.replace('\n', ''))
                a=list(node)
            for child in list(node):
                self.recursiveCollectET(child, result,only_sections)
        return result


    def list2text(self, lst):
        result = ''
        for x in lst:
            result += x.strip() + ' '
        return result.strip()
    #end
#end


########################################
### ALL LEUKEMIA SEARCH
#a = NCBI_Extractor()
##d = a.getDocument(2792210)
#ids = a.query('("t-lymphocytes"[MeSH Terms] OR "t-lymphocytes"[All Fields] OR "t cell"[All Fields] OR "t-cell"[All Fields]) OR ("leukaemia"[All Fields] OR "leukemia"[MeSH Terms] OR "leukemia"[All Fields])',
#              maxHits=1001)

#ids = a.query('leukemia', maxHits=10)
#fp = open('ALL-ids.pickle', 'wb')
#cPickle.dump(ids, fp, cPickle.HIGHEST_PROTOCOL)
#fp.close()
########################################