import sys
import urllib2
import json
import time

ENTREZ_PREFIX = 'EntrezGene:'
GO_PREFIX = 'GO:'
KEGG_PREFIX = 'KEGG:'

BIOMINE_KEGG_PREFIX_HSA = 'KEGG:hsa'
BIOMINE_KEGG_PREFIX_ATH = 'KEGG:ath'
BIOMINE_KEGG_PREFIX_STU = 'KEGG:stu'

GOMAPMAN_GENE_PREFIX = 'GoMapMan:'
BMPREFIX = 'GoMapMan/Gene:'

FSLASH = '/'
COL = ':'
CR = '\r'
LATEST_DB_VERSION = '<latest>'

# bmg graph file constants
GROUP = '_group'
ATTRIBUTES = '_attributes'
FILL = 'fill'

DEMO_URL = 'http://biomine.cs.helsinki.fi/demo'
HSA_BASE_URL = 'http://biomine.cs.helsinki.fi/search/'
PLANTS_BASE_URL = 'http://biomine.cs.helsinki.fi/plants/'

QUERY_CGI = 'query.cgi'
DOWNLOAD_CGI = 'download.cgi'
STATUS_CGI = 'status.cgi'
SEARCHES = 'searches'
FILE = 'file'
FILES = 'files'
MEDOIDS_KEY = 'Medoids'
NAME_KEY = 'name'
FILES_KEY = 'files'
MEDOID_MARK = 'medoid'
BEST_PATHS = 'Best paths'

START_NODES = 'start_nodes'
END_NODES = 'end_nodes'
MAX_NODES = 'max_nodes'
GROUPING = 'grouping'
FIND_MEDOIDS = 'find_medoid'
SINGLE_COMPONENT = 'single_component'
DATABASE = 'database'
QUERY_END = 'q=1'


STATUS = 'status'
MESSAGES = 'messages'
QUERY = 'query'
RESULT = 'result'
ERRORS = 'errors'
WARNING = 'Warning'
ERROR = 'Error'

STATUS_RUNNING = 'running'
STATUS_ERROR = 'error'
STATUS_ERRORS = 'errors'
STATUS_COMPLETED = 'completed'


class BiomineSearchError(Exception):
    pass

class BiomineSearch:
    BASE_URL = DEMO_URL

    def __init__(self, groupNodes=False, singleComponent=False, maxNodes=0, 
                 medoids=False, startNodes=[], endNodes=[],
                 databaseVersion=LATEST_DB_VERSION):
        self.groupNodes = 1 if groupNodes else 0
        self.singleComponent = 1 if singleComponent else 0
        self.maxNodes = maxNodes
        self.medoids = 1 if medoids else 0
        self.startNodes = startNodes
        self.endNodes = endNodes
        self.databaseVersion = databaseVersion

    def setSingleComponent(self, state):
        if state:
            self.singleComponent = True
        else:
            self.singleComponent = False

    def setMaxNodes(self, val):
        if val > 0:
            self.maxNodes = val
        else:
            self.maxNodes = None

    def setGrouping(self, state):
        if state:
            self.groupNodes = True
        else:
            self.groupNodes = False

    def buildResultURL(self, jobID, jobStatus):
        url = '%s/%s/%s/%s' % (self.BASE_URL, SEARCHES, jobID, jobStatus[RESULT])
        return url

    def buildBiomineQuery(self, startNodes, maxNodes=None, endNodes=None, 
                          medoids=0, grouping=0, singleComponent=0, dbVersion=''):
        def makeQueryString(parts):
            body = ''
            for elt in parts:
                for ch in str(elt):
                    if (ch == FSLASH) or (ch == COL):
                        ch = '%' + hex(ord(ch))[2:].upper()
                    body += ch
                body += '%0' + hex(ord(CR))[2:].upper()
            return body

        if startNodes:
            body = '%s=%s' % (START_NODES, makeQueryString(startNodes))
            if endNodes:
                body += '&%s=%s' % (END_NODES, makeQueryString(endNodes))
        elif endNodes:
            body = '%s=%s' % (END_NODES, makeQueryString(endNodes))
        else:
            raise BiomineSearchError('No nodes to query')

        if maxNodes:
            body += '&%s=%d' % (MAX_NODES, maxNodes)
        if medoids:
            body += '&%s=%d' % (FIND_MEDOIDS, 1)
        if dbVersion:
            body += '&%s=%s' % (DATABASE, dbVersion)
        body += '&%s=%d' % (GROUPING, grouping)
        body += '&%s=%d' % (SINGLE_COMPONENT, singleComponent)
        body += '&%s' % QUERY_END
        return body

    def biomineSearch(self, queryBody, timeout=240):
        url = '%s/%s?%s' % (self.BASE_URL, QUERY_CGI, queryBody)
        jobPost = json.load(urllib2.urlopen(urllib2.Request(url)))
        jobID = jobPost[QUERY]

        # now pool in the loop until the job is done or until timeout or until error
        statusUrl = '%s/%s?%s&q=1' % (self.BASE_URL, STATUS_CGI, jobID)
        startTime = time.time()
        cnt = 0
        while True:
            if cnt >= 20:
                cnt = 0
                jobStatus = json.load(urllib2.urlopen(urllib2.Request(statusUrl)))
                if jobStatus[STATUS] == STATUS_RUNNING:
                    #self.progressBar.advance()
                    # TODO add clowdflows progress bar
                    pass
                elif (jobStatus[STATUS] == STATUS_ERROR) or \
                     (jobStatus[STATUS] == STATUS_ERRORS):
                    msgs = ''
                    for elt in jobStatus[ERRORS]:
                        for msg in elt[MESSAGES]:
                            msgs += msg + '\n'
                    for ms in jobStatus[MESSAGES]:
                        msgs += '%s\n' % ms
                    raise BiomineSearchError(msgs)
                elif jobStatus[STATUS] == STATUS_COMPLETED:
                    resultUrl = self.buildResultURL(jobID, jobStatus)
                    bmgFile = urllib2.urlopen(urllib2.Request(resultUrl)).read()
                    bestPath = None
                    for elts in jobStatus[FILES]:
                        if NAME_KEY in elts:
                            if elts[NAME_KEY] == BEST_PATHS:
                                url = '%s/%s/%s/%s' % (self.BASE_URL, SEARCHES, 
                                                       jobID, elts[FILE])
                                bestPath = urllib2 \
                                           .urlopen(urllib2.Request(url)) \
                                           .read()
                    break
                else:
                    raise BiomineSearchError('Unknown Biomine search status: %s' 
                                             % jobStatus[STATUS])
            else:
                cnt += 1
                time.sleep(0.05)

            if (time.time() - startTime) > timeout:
                raise BiomineSearchError('Timeout while waiting for BioMine')
        return bmgFile, bestPath

    def invokeBiomine(self):
        # if database version is not specified rely on 
        # the Biomine system to use the latest
        dbVer = self.databaseVersion
        if self.databaseVersion == LATEST_DB_VERSION:
            dbVer = ''
        q = self.buildBiomineQuery(startNodes=self.startNodes, 
                                   endNodes=self.endNodes, 
                                   medoids=self.medoids,
                                   maxNodes=self.maxNodes, 
                                   grouping=self.groupNodes, 
                                   singleComponent=self.singleComponent,
                                   dbVersion=dbVer)
        try:
            result, bestPath = self.biomineSearch(q)
        except Exception, e:
            raise BiomineSearchError('Error while calling Biomine:\n%s' % str(e))
        else:
            self.consumeInput()
            return result, bestPath

    def consumeInput(self):
        self.startNodes = None
        self.endNodes = None

if __name__ == '__main__':
    nodes = ["EntrezGene:25163",
             "EntrezGene:25203",
             "EntrezGene:29685",
             "EntrezGene:29728",
             "EntrezGene:171576",
             "EntrezGene:291885",
             "EntrezGene:297176",
             "EntrezGene:314949"]
    search = BiomineSearch(startNodes=nodes)
    res, g = search.invokeBiomine()
    print res, g
