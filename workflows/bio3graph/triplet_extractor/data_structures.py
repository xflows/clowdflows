from os.path import normpath, join, dirname
import os
import codecs
from unidecode import unidecode
import nltk
from tagger import geniatagger
import pickle

MAX_WORDLEN = 300

PASSIVE_AUX_VERBS = set(['be', 'is', 'are', 'was', 'were', 'has been', 'have been', 'had been', 'get', 'got'])
ACTIVATE = 'activates'
BIND = 'binds'
INHIBIT = 'inhibits'


NP_delete = set(['biosynthesis inhibitor','signalling','signaling', 'homolog',
                 'signal','signals','pathway','pathways','induction','regulation', 'response',
                 'responses','responsive','inhibitor','inhibitors','activator','activators',
                 'producer','producers','plant','plants','mutant','mutants','mutation','mutations',
                 'line','lines'])
VP_delete = set(['may','might','can','could','would'])
sentence_delete = set([x.lower() for x in ['whether','To determine','To investigate','To study',
                                           'one possibility','would be that','It was postulated']])


class Document(object):
    def __init__(self, fname=None, docid='', utfdecode=False):
        self.docid = docid
        self.fname = fname
        self.rawText = None
        self.rawSentences = None
        self.tokenizedSentences = None
        self.taggedSentences = None
        self.parsedSentences = None

        if self.fname:
            if not self.docid:
                self.docid = self.fname
            self.loadFile(self.fname, utfdecode=utfdecode)
    #end

    def loadFile(self, fname, utfdecode=False):
        if utfdecode:
            self.rawText = unidecode(codecs.open(fname, 'r', 'utf-8').read())
        else:
            self.rawText = codecs.open(fname, 'r', 'utf-8').read()
        self.fname = fname
        if not self.docid:
            self.docid = self.fname
    #end

    def loadString(self, s):
        self.rawText = s

    def setID(self, s):
        self.docid = s
#end class


class Corpus(list):
    def loadFromDirectory(self, dirname, ftypes=['.txt']):
        fnames = os.listdir(dirname)
        for fname in fnames:
            fullName = os.path.join(dirname, fname)
            if os.path.splitext(fname)[1] in ftypes and os.path.isfile(fullName):
                self.append(Document(fname=fullName))
    #end
#end class


class Triplet(object):
    def __init__(self, subject, predicate, object, sentence, document=None, sentenceNumber=None):
        self.subject = subject
        self.original_subject = subject
        self.predicate = predicate
        self.original_predicate = predicate
        self.object = object
        self.original_object = object
        self.sentence = sentence
        self.__sentenceNumber = sentenceNumber
        self.document = document
        self.passive_aux_verbs = []
        self.passive = None

        self.documentID = ''
        if self.document:
            self.setDocumentID(self.document.docid)
    #end

    def setDocumentID(self, did):
        self.documentID = did


    def __eq__(self, other):
        if self.predicate == other.predicate:
            if self.subject == other.subject and self.object == other.object:
            #if self.subject == other.subject and self.object == other.object or \
               #self.subject == other.object and self.object == other.subject:
                return True
            else:
                return False
        else:
            return False
    #end

    def __cmp__(self, other):
        if self.subject < other.subject:
            return -1
        elif self.subject > other.subject:
            return 1
        else:
            if self.predicate < other.predicate:
                return -1
            elif self.predicate > other.predicate:
                return 1
            else:
                if self.object < other.object:
                    return -1
                elif self.object > other.object:
                    return 1
                else:
                    return 0
    #end


    def __hash__(self):
        return hash('%s%s%s' % (self.subject, self.predicate, self.object))


    def __str__(self):
        return '(%s, %s, %s)' % (self.subject, self.predicate, self.object)
        #if self.passive_aux_verbs == []:
            #return '(%s, %s, %s)' % (self.subject, self.predicate, self.object)
        #else:
            #return '(%s, [%s]%s, %s)' % (self.subject, self.passive_aux_verbs[0], self.predicate, self.object)
    #end

    def __repr__(self):
        return self.__str__()


    def printOriginal(self, fp=None):
        if self.passive_aux_verbs == []:
            s = '(%s, %s, %s)' % (self.original_subject, self.original_predicate, self.original_object)
            #print '(%s, %s, %s)' % (self.original_subject, self.original_predicate, self.original_object)
        else:
            s = '(%s, %s%s, %s)' % (self.original_subject, str(self.passive_aux_verbs), self.original_predicate, self.original_object)
            #print '(%s, %s%s, %s)' % (self.original_subject, str(self.passive_aux_verbs), self.original_predicate, self.original_object)
        return s
    #end


    def getParsedSentence(self):
        if self.__sentenceNumber and self.document:
            return self.document.parsedSentences[self.__sentenceNumber]
        else:
            return None
    #end
#end


class SentenceSplitter(object):
    def splitNLTK(self, document):
        assert(isinstance(document, Document))

        ###########
        for ch in [' ', "'", ')', '\n', ',', '.', '[', ':', ';']:
            document.rawText = document.rawText.replace(' et al' + ch, ' ETAL.')
        document.rawText = document.rawText.replace('SA-treatment', 'SA treatment')
        document.rawText = document.rawText.replace('H 2 O 2', 'H2O2')
        ###########


        tokenizer = pickle.load(open(normpath(join(dirname(__file__),'punkt/english.pickle'))))
        #tokenizer = pickle.load(open(os.path.normpath('punkt/english.pickle')))
        document.rawSentences = [s.replace('\n', '') for s in tokenizer.tokenize(document.rawText)]
    #end

    def splitGENIA(self, document):
        assert(isinstance(document, Document))
        raise NotImplementedError
    #end
#end class


class GeniaTTC(object):
    '''GENIA tokenizer, tagger and chunker'''
    def __init__(self, loadModels=True):
        if loadModels:
            cd = os.getcwd()
            os.chdir(normpath(join(dirname(__file__),'tagger')))
            geniatagger.load_models()
            os.chdir(cd)
    #end

    def process(self, document):
        assert(isinstance(document, Document))

        tokenizedSentences = []
        taggedSentences = []
        parsedSentences = []
        for sentence in document.rawSentences:
            # shorten word longer than
            words = sentence.split()
            words = [w[:MAX_WORDLEN] for w in words]
            sent = ''
            for w in words:
                sent += w + ' '

            #print repr(sentence)
            #print repr(sent)
            #print
            sentence = sent

            tokenized = []
            tokenizedTagged = []
            parsed = []

            if type(sentence) == unicode:
                geniaResult = geniatagger.tag_sentence(sentence.encode('utf-8'))
            else:
                geniaResult = geniatagger.tag_sentence(sentence)

            for wordTags in geniaResult:
                word, base, POStag, chunktag, NEtag = wordTags[0], wordTags[1], wordTags[2], wordTags[3], wordTags[4]
                tokenized.append(word)
                tokenizedTagged.append((word, POStag))
                parsed.append((word, POStag, chunktag))
            #end
            tokenizedSentences.append(tokenized)
            taggedSentences.append(tokenizedTagged)
            #parsedSentences.append(nltk.chunk.util.conlltags2tree(parsed))
            parsedSentences.append(parsed)
        #end
        document.tokenizedSentences = tokenizedSentences
        document.taggedSentences = taggedSentences
        document.parsedSentences = parsedSentences
    #end

    def processSentence(self, sentence):
        tokenized = []
        tokenizedTagged = []
        parsed = []
        geniaResult = geniatagger.tag_sentence(sentence)
        for wordTags in geniaResult:
            word, base, POStag, chunktag, NEtag = wordTags[0], wordTags[1], wordTags[2], wordTags[3], wordTags[4]
            tokenized.append(word)
            tokenizedTagged.append((word, POStag))
            parsed.append((word, POStag, chunktag))
        #end
        #return tokenized, tokenizedTagged, nltk.chunk.util.conlltags2tree(parsed)
        return tokenized, tokenizedTagged, parsed
    #end

    def tokenizeSentence(self, sentence):
        return self.processSentence(sentence)[0]

    def tagSentence(self, sentence):
        return self.processSentence(sentence)[1]

    def parseSentence(self, sentence):
        return self.processSentence(sentence)[2]

#end class



class Vertex(object):
    def __init__(self, name, longName=''):
        self.name = name
        self.longName = longName

    def __hash__(self):
        return hash(self.name)
#end class


class Arc(object):
    def __init__(self, start, end, name, sentence=''):
        assert(isinstance(start, Vertex))
        assert(isinstance(end, Vertex))
        self.start = start
        self.end = end
        self.name = name
        self.sentence = sentence
#end class



if __name__ == "__main__":
    #a = Document(fname='/home/vid/programiranje/wingIDE_projects/dragana1/vid/testdoc2.txt')
    #SentenceSplitter().splitNLTK(a)
    #p = GeniaTTC().parseSentence('The generated ROS activate the production of ethylene, JA and SA.')
    #t, tt, p = GeniaTTC().processSentence('The pathogen-inducible genes PR-1 , PR-2 , and PR-5 require SA signaling for activation, whereas the plant defensin gene PDF1.2 , along with a PR-3 and PR-4 gene, are induced by pathogens via an SA-independent and JA-dependent pathway.')
    #t, tt, p = GeniaTTC().processSentence('In wild-type Col-0 plants, exogenous application of SA activated PR-1, whereas treatment with methyl jasmonate (MeJA) resulted in the accumulation of LOX2 , VSP , and PDF1.2 mRNA ( Figure 3 ).')
    t, tt, p = GeniaTTC().processSentence('Treatment with methyl jasmonate (MeJA) resulted in the accumulation of LOX2 , VSP , and PDF1.2 mRNA ( Figure 3 ).')
    a = nltk.chunk.util.conlltags2tree(p)

    d = Document()
    d.loadString('Treatment with methyl jasmonate (MeJA) resulted in the accumulation of LOX2 , VSP , and PDF1.2 mRNA ( Figure 3 ). A sorghum homolog of the AIM1 gene from Arabidopsis, encoding a fatty acid oxidase ( Richmond and Bleecker, 1999 ), an acyl-CoA synthetase, and an acyl-CoA oxidase, were also confirmed by qRT-PCR to be induced by both SA and MeJA. This is a test.')
    ss = SentenceSplitter()
    ss.splitNLTK(d)
    print d.tokenizedSentences




#A sorghum homolog of the AIM1 gene from Arabidopsis, encoding a fatty acid oxidase ( Richmond and Bleecker, 1999 ), an acyl-CoA synthetase, and an acyl-CoA oxidase, were also confirmed by qRT-PCR to be induced by both SA and MeJA.