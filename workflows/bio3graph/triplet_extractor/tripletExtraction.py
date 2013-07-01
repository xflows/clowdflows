import os
from xml.etree.ElementTree import TreeBuilder, ElementTree, Element, SubElement, tostring
from xml.dom.minidom import parseString
import copy
import cStringIO, StringIO
import nltk
import data_structures
import csv

import networkx as nx
import graph_operations as gop



def searchDocuments(folder, word):
    files = os.listdir(folder)
    for f in files:
        fullname = os.path.join(folder, f)
        if os.path.isfile(fullname) and os.path.splitext(f)[1] == '.txt':
            text = open(fullname).read()
            found = find_substring_all(word, text)
            if found:
                #print f, found
                print fullname
#end


def readEntitiesLnDoc(fname):
    '''This function reads compound names from a file.
    Each line is either empty, or contains synonym(s) for some entity.
    Returns a dictionary where keys are base names and values are synonyms.
    '''
    entities = {}
    for line in open(fname):
        if not line:
            break
        line = line.strip().lower()
        if line:
            elts = [x.strip() for x in line.split(',')]
            elts = [x for x in elts if x]
            entities[elts[0]] = list(set(elts[1:]))
    #end
    return entities
#end


def readEntitiesLnDoc_csv(fname):
    '''This function reads compound names from a file.
    Each line is either empty, or contains synonym(s) for some entity.
    Returns a dictionary where keys are base names and values are synonyms.
    '''

    reader = csv.reader(open(fname), skipinitialspace=True)
    entities = {}
    for row in reader:
        if len(row) == 0:
            continue
        name = row[0].lower()
        syns = [x.strip().lower() for x in row[1:]]
        entities[name] = list(set(syns))
    #end
    return entities
#end



def readEntitiesLnDoc_stringIO(s):
    '''This function reads compound names from a stringIO object.
    Each line is either empty, or contains synonym(s) for some entity.
    Returns a dictionary where keys are base names and values are synonyms.
    '''
    entities = {}
    s.seek(0)
    for line in s:
        if not line:
            break
        line = line.strip().lower()
        if line:
            elts = [x.strip() for x in line.split(',')]
            elts = [x for x in elts if x]
            entities[elts[0]] = list(set(elts[1:]))
    #end
    return entities
#end



def readLnEntities(fname):
    '''This function reads compound names from a file.
    Each line is either empty, or contains an entity or entities separated by commas
    Returns a list of entities
    '''
    entities = []
    for line in open(fname):
        if not line:
            break
        line = line.strip().lower()
        if line:
            elts = [x.strip() for x in line.split(',')]
            elts = [x for x in elts if x]
            entities.extend(elts)
        #end
    return entities
#end


def readLnEntities_stringIO(s):
    '''This function reads compound names from a stringIO object
    Each line is either empty, or contains an entity or entities separated by commas
    Returns a list of entities
    '''
    entities = []
    s.seek(0)
    for line in s:
        if not line:
            break
        line = line.strip().lower()
        if line:
            elts = [x.strip() for x in line.split(',')]
            elts = [x for x in elts if x]
            entities.extend(elts)
        #end
    return entities
#end



def readNames(fname):
    '''This function reads names from a file.
    Each line is either empty, or contains some name.
    Returns a list of names.
    '''
    data = []
    for line in open(fname):
        if not line:
            break
        elt = line.strip().lower()
        if elt:
            data.append(elt)
    return list(set(data))
#end


def removeUnchunked(tree):
    leaves = [x for x in tree if isinstance(x, nltk.tree.Tree)]
    return nltk.tree.Tree(tree.node, children=leaves)


# is full traversal needed at all i.e. depth >2 ??
def getPhrases(root, ptype):
    phrases = []
    for elt in root:
        if isinstance(elt, nltk.tree.Tree): #if not leaf
            if elt.node == ptype:
                phrases.append(elt)
            #for child in elt:
                #phrases.append(getPhrases(child, ptype))
    return phrases
#end


def getPhrasesText(root):
    text = ''
    for elt in root:
        if isinstance(elt, nltk.tree.Tree): #if not leaf
            text += getPhrasesText(elt) + ' '
        else:
            text += elt[0] + ' '
    return text.strip().lower()
#end


def getPhrasesText_list(root):
    text = []
    for elt in root:
        if isinstance(elt, nltk.tree.Tree): #if not leaf
            text.extend(getPhrasesText_list(elt))
        else:
            text.append(elt[0].lower())
    return text
#end


def fill_KMP_table(T, W):
    pos = 2
    cnd = 0

    T[0] = -1
    T[1] = 0
    while pos < len(W):
        if W[pos-1] == W[cnd]:
            cnd += 1
            T[pos] = cnd
            pos += 1
        elif cnd > 0:
            cnd = T[cnd]
        else:
            T[pos] = 0
            pos += 1
#end


# A straightforward implementation of the Knuth-Morris-Pratt algorithm.
# Note: this version works on lists
# The algorithm searches for the first occurence of W in S
#
def KMP(W, S):
    m = 0
    i = 0

    # marginal cases
    if len(W) == 0:
        return -1
    if len(W) == 1:
        return S.index(W)

    T = [0]*len(W) # make a zero array
    fill_KMP_table(T, W)

    while m+i < len(S):
        if W[i] == S[m+i]:
            if i == len(W)-1:
                return m
            i += 1
        else:
            m = m + i - T[i]
            if T[i] > -1:
                i = T[i]
            else:
                i = 0
    return -1 # not found
#end


def take(data, indices):
    '''Returns list of elements using indices'''
    new = []
    for i in indices:
        new.append(data[i])
    return new
#end


def find_sublist(sub, data):
    ''' This is an equivalent for .index() but searches for sublist'''
    if len(sub) > len(data):
        return -1

    pos = 0
    while pos < len(data):
        try:
            i = data[pos:].index(sub[0])
        except ValueError:
            return -1

        if data[pos+i:pos+i+len(sub)] == sub:
            return pos+i
        else:
            pos += i + 1
    #endwhile
    return -1
#end


def find_sublist_all(sub, data):
    ''' Searches for all occurences of sublist sub in a given list'''
    if (len(sub) > len(data)) or (sub == []):
        return []

    found = []
    pos = 0
    while pos < len(data):
        try:
            i = data[pos:].index(sub[0])
        except ValueError:
            break

        if data[pos+i:pos+i+len(sub)] == sub:
            found.append(pos + i)
        pos += i + 1
    #endwhile
    return found
#end


def find_element_all(elt, data):
    ''' This is an equivalent for .index() but finds all occurences'''
    found = []
    pos = 0
    while pos < len(data):
        try:
            i = data[pos:].index(elt)
        except ValueError:
            break

        found.append(pos + i)
        pos += i + 1
    #endwhile
    return found
#end


def find_substring_all(s, text):
    ''' This is an equivalent for "text.find(s) but finds all non-overlapping occurences'''
    if len(s) == 0 or len(s) > text:
        return []

    found = []
    pos = 0
    while pos < len(text):
        i = text[pos:].find(s)
        if i == -1:
            break
        else:
            found.append(pos+i)
            pos += i + len(s)
    #end
    return found
#end


class TripletGraphConstructor(object):
    def __init__(self, triplets):
        self.triplets = triplets
    #end

    def export_networkx(self, nodePrefix='component', sentenceAttr=False, docidAttr=False):
        g = nx.MultiDiGraph()
        for tr in self.triplets:
            assert (isinstance(tr, data_structures.Triplet))
            node1 = nodePrefix + '_' + tr.subject.replace(' ','+')
            node2 = nodePrefix + '_' + tr.object.replace(' ','+')

            g.add_node(node1)
            g.add_node(node2)
            g.add_edge(node1, node2, tr.predicate.replace(' ','+'))
            if sentenceAttr:
                g.edge[node1][node2][tr.predicate]['sentence'] = tr.sentence.replace(' ','+')
            if docidAttr:
                g.edge[node1][node2][tr.predicate]['documentID'] = os.path.split(tr.documentID)[1]
        return g
     #G.node[nodeid][attr] = ...
     #G.edge[fr][to][attr] = ...
    #end

    def exportBMG(self, nodePrefix='component', sentenceAttr=False, docidAttr=False):
        g = self.export_networkx(nodePrefix, sentenceAttr, docidAttr)
        s = gop.export_to_BMG(g)
        return s
    #end


    def exportBMG_OLD(self, predicateAsNode=False):
        graph = StringIO.StringIO()
        data = {}
        nodes = {}

        # get data into a dict
        for (i, t) in enumerate(self.triplets):
            assert(isinstance(t, data_structures.Triplet))
            if t.subject not in data:
                data[t.subject] = {}
            if t.object not in data[t.subject]:
                data[t.subject][t.object] = {}
            if t.predicate not in data[t.subject][t.object]:
                data[t.subject][t.object][t.predicate] = {'sentences':[], 'docids':[]}

            data[t.subject][t.object][t.predicate]['sentences'].append(t.sentence)
            data[t.subject][t.object][t.predicate]['docids'].append(t.documentID)
        #end

        ## collect all node names
        #for sub in data:
            #nodes[sub] = sub
            #for obj in data[sub]:
                #if obj not in nodes:
                    #nodes[obj] = obj

        # get all connections
        lines = []
        for sub in data:
            for obj in data[sub]:
                for pred in data[sub][obj]:
                    sents = ''
                    for s in data[sub][obj][pred]['sentences']:
                        sents += s.replace('\n', '').replace(' ','+')

                    #ids = [self.filename2pubmedID(x) for x in data[sub][obj][pred]['docids']]
                    idlst = str(data[sub][obj][pred]['docids'])[1:-1]
                    if idlst == '':
                        idlst = 'Unknown'
                    lines.append('component_%s component_%s %s Sentences=%s SourceID=%s\n' % (sub.replace(' ',':'), obj.replace(' ','+'), pred, sents, idlst.replace(' ','+')))
        #end

        graph.writelines(lines)
        #graph.writelines(['# _attributes compound_%s PrimaryName=%s\n' % (node.replace(' ','+'), node.replace(' ','+')) for node in nodes])

        return graph.getvalue()

        #for (i, t) in enumerate(self.triplets):
            #sub = t.subject.replace(' ','_')
            #pred = t.predicate.replace(' ','_')
            #obj = t.object.replace(' ','_')
            #sent = 'Sentence=' + t.sentence.replace(' ','+')  if t.sentence else ''
            #lines = []
            #if predicateAsNode:
                #lines.append('compound_%s %d_%s\n' % (sub, i, pred))
                #lines.append('%d_%s compound_%s\n' % (i, pred, obj))
                #lines.append('# _attributes %d_%s %s\n' % (i, pred, sent))
                #lines.append('# _attributes compound_%s queryset=end\n' % sub)
                #lines.append('# _attributes compound_%s queryset=end\n' % obj)
            #else:
                #lines.append('compound_%s compound_%s %s %s\n' % (sub, obj, pred, sent))
            #graph.writelines(lines)
        #return graph.getvalue()
    #end


    def exportText(self):
        out = cStringIO.StringIO()
        for t in self.triplets:
            if t.documentID:
                out.write(self.filename2pubmedID(t.documentID) + '\n')
            out.write(str(t) + '\n')
            #out.write('(%s, %s, %s)' % (t.original_subject, t.original_predicate, t.original_object) + '\n')
            out.write(t.sentence + '\n\n')
        return out.getvalue()
    #end


    def exportXML(self):
        root = Element('triplets')
        for t in self.triplets:
            assert isinstance(t, data_structures.Triplet)
            new = SubElement(root, 'triplet')

            s = SubElement(new, 'subject')
            s.text = t.subject

            p = SubElement(new, 'predicate')
            p.text = t.predicate

            o = SubElement(new, 'object')
            o.text = t.object

            st = SubElement(new, 'sentence')
            st.text = t.sentence

            if t.documentID:
                did = SubElement(new, 'documentID')
                did.text = self.filename2pubmedID(t.documentID)
        #end for
        return parseString(tostring(root)).toprettyxml()
    #end

    def filename2pubmedID(self, fname):
        try:
            fname = os.path.splitext(os.path.split(fname)[1])[0].upper()
        except Exception:
            return fname
        return fname
    #end

    def interactome2biomine(self, csvData):
        csvIO = StringIO.StringIO()
        csvIO.write(csvData)
        csvIO.seek(0)
        interactome_csv = csv.reader(csvIO, dialect='excel', delimiter=';', quotechar='|')

        unique_triplets_raw = []
        for column in interactome_csv:
            node1 = column[2]
            node2 = column[3]
            element = (node1, node2)
            element2 = (node2, node1)
            unique_triplets_raw.append(element)
            unique_triplets_raw.append(element2)
        #end
        unique_triplets_raw = list(set(unique_triplets_raw))  #remove duplicates


        #print "unique list of all my triplets is ", str (unique_triplets_raw)
        #print "duzina liste je ", str (len(unique_triplets_raw))

        edges = ''
        node_attributes = ''
        fBiomine = StringIO.StringIO()
        for element in unique_triplets_raw:
            node1 = element [0]
            node2 = element [1]
            line_biom1= "node_" + node1 + " node_" + node2 + " B\n"
            edges = edges + line_biom1
            line_biom2= "#_attribute node_" + node1 + "\n"
            line_biom3= "#_attribute node_" + node2 + "\n"
            node_attributes = node_attributes + line_biom2 + line_biom3

            #node_sa node_ja inhibits
            #_attributes node_sa fill=135/206/250
            #_attributes node_ja fill=135/206/250

        fBiomine.write(edges)
        fBiomine.write(node_attributes)
        fBiomine.flush()
        return fBiomine.getvalue()
    #end

#end class



class Vocabulary(object):
    def __init__(self):
        self.activations = None
        self.activations_all = None
        self.activations_passive = None
        self.activations_rotate = None

        self.inhibitions = None
        self.inhibitions_all = None
        self.inhibitions_passive = None

        self.bindings = None
        self.bindings_all = None
        self.bindings_passive = None

        self.passive_all = None

        self.predicates = None
        self.predicates_tokenized = None
        self.predicates_parsed = None
        self.allCompounds = None
        self.allCompounds_tokenized = None
        self.compoundSynonyms = None
    #end


    @staticmethod
    def dictCMP(a, b):
        if len(a) > len(b):
            return 1
        elif len(a) == len(b):
            return 0
        else:
            return -1
    #end

    def _buildCompoundsStructures(self, compounds):
        assert isinstance(compounds, dict)

        self.allCompounds = set(compounds.keys())
        for elt in compounds.values():
            self.allCompounds.update(elt)
        self.allCompounds = sorted(list(self.allCompounds), cmp=Vocabulary.dictCMP, reverse=True)
        self.allCompounds_tokenized = [nltk.word_tokenize(comp) for comp in self.allCompounds]

        self.compoundSynonyms = {}
        for comp in compounds.keys():
            for syn in compounds[comp]:
                self.compoundSynonyms[syn] = comp
            self.compoundSynonyms[comp] = comp
        #end
    #end

    def _buildVocabularyStructures(self):
        self.activations_all = dict.fromkeys(self.activations.keys() + self.activations_rotate.keys() + self.activations_passive.keys())
        self.inhibitions_all = dict.fromkeys(self.inhibitions.keys() + self.inhibitions_passive.keys())
        self.bindings_all = dict.fromkeys(self.bindings.keys() + self.bindings_passive.keys())


        self.passive_all = dict.fromkeys(self.activations_passive.keys() +
                                         self.inhibitions_passive.keys() +
                                         self.bindings_passive.keys())

        self.predicates = list(set(self.activations.keys() + self.activations_rotate.keys() + self.activations_passive.keys() +
                                   self.inhibitions.keys() + self.inhibitions_passive.keys() +
                                   self.bindings.keys() + self.bindings_passive.keys()))

        gt = data_structures.GeniaTTC()
        self.predicates_tokenized = {}
        self.predicates_parsed = {}
        for p in self.predicates:
            tokenized, tokenizedTagged, parsed = gt.processSentence(p)

            self.predicates_tokenized[p] = tokenized
            self.predicates_parsed[p] = parsed

            #self.predicates_tokenized = [nltk.word_tokenize(x) for x in self.predicates]
            #end
    #end


    def loadCompounds_stringIO(self, compString):
        compounds = readEntitiesLnDoc_stringIO(compString)
        self._buildCompoundsStructures(compounds)
    #end

    def loadCompounds_file(self, fname):
        compounds = readEntitiesLnDoc_csv(fname)
        self._buildCompoundsStructures(compounds)
    #end

    def loadCompounds_dict(self, compounds):
        self._buildCompoundsStructures(compounds)
    #end


    def loadPredicates_list(self,
                            activations = None,
                            activations_passive = None,
                            activations_rotate = None,
                            inhibitions = None,
                            inhibitions_passive = None,
                            bindings = None,
                            bindings_passive = None,
                            ):
        self.activations = dict.fromkeys(activations)
        self.activations_passive = dict.fromkeys(activations_passive)
        self.activations_rotate = dict.fromkeys(activations_rotate)

        self.inhibitions = dict.fromkeys(inhibitions)
        self.inhibitions_passive = dict.fromkeys(inhibitions_passive)

        self.bindings = dict.fromkeys(bindings)
        self.bindings_passive = dict.fromkeys(bindings_passive)

        #self._buildVocabularyStructures()
    #end

    def loadPredicates_stringIO(self,
                                activations = None,
                                activations_passive = None,
                                activations_rotate = None,
                                inhibitions = None,
                                inhibitions_passive = None,
                                bindings = None,
                                bindings_passive = None,
                                ):
        self.activations = dict.fromkeys(readLnEntities_stringIO(activations))
        self.activations_passive = dict.fromkeys(readLnEntities_stringIO(activations_passive))
        self.activations_rotate = dict.fromkeys(readLnEntities_stringIO(activations_rotate))

        self.inhibitions = dict.fromkeys(readLnEntities_stringIO(inhibitions))
        self.inhibitions_passive = dict.fromkeys(readLnEntities_stringIO(inhibitions_passive))

        self.bindings = dict.fromkeys(readLnEntities_stringIO(bindings))
        self.bindings_passive = dict.fromkeys(readLnEntities_stringIO(bindings_passive))

        #self._buildVocabularyStructures()
    #end


    def loadPredicates_files(self,
                                activationFname='',
                                activationFname_passive='',
                                activations_rotate = '',
                                inhibitionFname='',
                                inhibitionFname_passive='',
                                bindingFname='',
                                bindingFname_passive='',
                                ):

        self.activations = dict.fromkeys(readLnEntities(activationFname))
        self.activations_passive = dict.fromkeys(readLnEntities(activationFname_passive))
        self.activations_rotate = dict.fromkeys(readLnEntities(activations_rotate))

        self.inhibitions = dict.fromkeys(readLnEntities(inhibitionFname))
        self.inhibitions_passive = dict.fromkeys(readLnEntities(inhibitionFname_passive))

        self.bindings = dict.fromkeys(readLnEntities(bindingFname))
        self.bindings_passive = dict.fromkeys(readLnEntities(bindingFname_passive))

        #self._buildVocabularyStructures()
    #end

#end class


class TripletExtractor(object):
    def __init__(self, voc):
        assert isinstance(voc, Vocabulary)
        self.vocabulary = voc
        self.vocabulary._buildVocabularyStructures()
    #end


    def searchDocuments(self, folder):
        files = os.listdir(folder)
        counterP = dict.fromkeys(self.vocabulary.predicates, 0)
        counterC = dict.fromkeys(self.vocabulary.allCompounds, 0)
        for f in files:
            print f
            fullname = os.path.join(folder, f)
            if os.path.isfile(fullname) and os.path.splitext(f)[1] == '.txt':
                text = open(fullname).read().lower()
                for predicate in self.vocabulary.predicates:
                    counterP[predicate] += len(find_substring_all(predicate, text))
                for compound in self.vocabulary.allCompounds:
                    counterC[compound] += len(find_substring_all(compound, text))
        return counterC, counterP
    #end


    ##TEZAVE: iskanje sestavljenih imen (problem presledkov)
    def findEntitiesRE(self, entities, sentence):
        found = []
        sentence = sentence.lower()
        #sentence = unicode(sentence.lower())
        for ent in entities:
            backup = sentence
            i = sentence.find(ent)
            #i = sentence.find(unicode(ent))
            flag = False
            while i != -1:
                # zacetek, in naslednji znak ni alfanumericen
                if i == 0 and not sentence[len(ent)].isalnum():
                    flag = True
                # konec, in predhodnji znak ni alfanumericen
                elif i + len(ent) == len(sentence) and not sentence[-len(ent)-1].isalnum():
                    flag = True
                # vmes, med ne-alfanumericnimi znaki
                elif not sentence[i-1].isalnum() and not sentence[i+len(ent)].isalnum():
                    flag = True

                # ce je najden, pa ne velja nobeden od zgornjih primerov, je bil najden del vecje besede (to ne steje)
                # torej iscemo dalje po nizu
                if not flag:
                    sentence = sentence[i + len(ent):]
                    i = sentence.find(ent)
                else:
                    break
            #end
            if flag:
                found.append(ent)
            sentence = backup

            print
        #end
        return found
    #end


    def extractTripletsRE(self, document):
        '''This function finds all triplets in the document of form SUBJECT PREDICATE OBJECT
        where all terms were already found to be present in the sentence.
        '''
        assert(isinstance(document, data_structures.Document))

        documentTriplets = []
        for (snum, sentence) in enumerate(document.rawSentences):
            # first, find entities (compounds, predicates)
            foundPredicates = self.findEntitiesRE(self.vocabulary.predicates, sentence)
            if foundPredicates:
                foundCompounds = self.findEntitiesRE(self.vocabulary.allCompounds, sentence)
                if not foundCompounds or len(foundCompounds) < 2:
                    continue
            else:
                continue

            sentenceTriplets = self.matchSPO(document, snum, foundCompounds, foundPredicates)
            if sentenceTriplets:
                documentTriplets.extend(sentenceTriplets)

        return documentTriplets
    #end


    def matchSPO(self, document, snum, foundCompounds, foundPredicates):
        sentence = document.rawSentences[snum].lower()
        sentenceTriplets = []
        for predicate in foundPredicates:
            for (i, comp1) in enumerate(foundCompounds):
                for (j, comp2) in enumerate(foundCompounds[i+1:]):

                    #TODO: vse pojavitve, ne samo prva
                    sub = sentence.find(comp1)
                    pred = sentence.find(predicate)
                    obj = sentence.find(comp2)

                    if self.vocabulary.compoundSynonyms[comp1] == self.vocabulary.compoundSynonyms[comp2]:
                        continue

                    if sub < pred and pred < obj:
                        sentenceTriplets.append(data_structures.Triplet(comp1, predicate, comp2, document.rawSentences[snum], document, snum))
                    elif obj < pred and pred < sub:
                        sentenceTriplets.append(data_structures.Triplet(comp2, predicate, comp1, document.rawSentences[snum], document, snum))
        return sentenceTriplets
    #end


    def filterTriplets(self, triplets):
        '''Filters out invalid triplets according to some simple rules'''
    #end



    def normalizeTriplets(self, triplets):
        '''Transforms all parts into base names'''

        normalized = []
        for triplet in triplets:
            new = copy.copy(triplet)

            passive = False
            if triplet.predicate in self.vocabulary.activations_all:
                pred = data_structures.ACTIVATE
                if triplet.predicate in self.vocabulary.activations_passive:
                    passive = True
            elif triplet.predicate in self.vocabulary.inhibitions_all:
                pred = data_structures.INHIBIT
                if triplet.predicate in self.vocabulary.inhibitions_passive:
                    passive = True
            elif triplet.predicate in self.vocabulary.bindings_all:
                pred = data_structures.BIND
                if triplet.predicate in self.vocabulary.bindings_passive:
                    passive = True
            else:
                print 'Invalid predicate: "%s"; document: %s' % (triplet.predicate, triplet.document.fname)
                continue

            # only if predicate is from passiv dict and there are aux verbs
            if passive and triplet.passive_aux_verbs != []:
                # switch subject and object
                subj = self.vocabulary.compoundSynonyms[new.object]
                obj = self.vocabulary.compoundSynonyms[new.subject]
                true_passive = True
            # or predicate is a rotation predicate
            elif triplet.predicate in self.vocabulary.activations_rotate:
                # switch subject and object
                subj = self.vocabulary.compoundSynonyms[new.object]
                obj = self.vocabulary.compoundSynonyms[new.subject]
                true_passive = False
            else:
                subj = self.vocabulary.compoundSynonyms[new.subject]
                obj = self.vocabulary.compoundSynonyms[new.object]
                true_passive = False

            new.subject = subj
            new.object = obj
            new.predicate = pred
            new.passive = true_passive

            normalized.append(new)
        #end

        return normalized
    #end


    def isConditionalSentence(self, tokenizedSentence):
        if data_structures.sentence_delete.intersection(set([x.lower() for x in tokenizedSentence])):
            return True
        else:
            return False
    #end


    def extractTripletsNLP(self, document, VP_CHECK_POS=None):
        assert(isinstance(document, data_structures.Document))
        assert(VP_CHECK_POS == 1 or VP_CHECK_POS == 2)

        documentTriplets = []
        for (snum, parsed) in enumerate(document.parsedSentences):

            # do not process conditional sentences (i.e. guesses and new ideas)
            # this does not work for the service as there is no separate attribute "tokenizedSentences"
            #if self.isConditionalSentence(document.tokenizedSentences[snum]):
            if self.isConditionalSentence([tags[0] for tags in document.parsedSentences[snum]]):
                #print 'Conditional: ', document.rawSentences[snum]
                continue

            original_tree = nltk.chunk.util.conlltags2tree(parsed)

            # collect all leaves of the parse tree into a list
            # and starting positions of leaf contents in the sentence
            # also collect positions of all tree parts
            leaves = []
            leavesPositions = []
            allLeaves = []
            allLeavesPositions = []
            pos = 0
            for x in original_tree:
                if isinstance(x, nltk.tree.Tree):
                    leaves.append(x.node)
                    leavesPositions.append(pos)
                    allLeaves.append(x.node)
                    allLeavesPositions.append(pos)
                    pos += len(x)
                else:
                    allLeaves.append(x[1])
                    allLeavesPositions.append(pos)
                    pos += 1
            #endfor

            sentTokens = getPhrasesText_list(original_tree)


            # remove punctuations etc. from the chunk tree
            # this should improve dictionary matching (tested: it does a lot!)
            #tree = removeUnchunked(tree)
            tree = removeUnchunked(original_tree)

            ## collect all leaves of the parse tree into a list
            ## and starting positions of leaf contents in the sentence
            #tree = removeUnchunked(tree)
            #leaves = []
            #leavesPositions = []
            #pos = 0
            #for x in tree:
                ##if isinstance(x, nltk.tree.Tree):
                #leaves.append(x.node)
                #leavesPositions.append(pos)
                #pos += len(x)
            ##endfor

            #sentTokens = getPhrasesText_list(tree)

            # get indices of all NPs and VPs
            NPs = find_element_all('NP', leaves)
            VPs = find_element_all('VP', leaves)
            vp_positions = take(leavesPositions, VPs)
            np_positions = take(leavesPositions, NPs)


            #NPs_full = find_element_all('NP', allLeaves)
            #VPs_full = find_element_all('VP', allLeaves)
            #vp_positions_full = take(allLeavesPositions, VPs_full)
            #np_positions_full = take(allLeavesPositions, NPs_full)


            # iterate through all triples <NP1, VP, NP2>
            # and search for matches against the dictionary
            for (vpNum, vp) in enumerate(VPs):
                vp_list = getPhrasesText_list(tree[vp])
                vp_set = set(vp_list)
                vpPos = vp_positions[vpNum]
                vpPos_next = vp_positions[vpNum+1] if vpNum+1 < len(vp_positions) else None
                vpPos_prev = vp_positions[vpNum-1] if vpNum-1 >= 0 else None

                vpPos_prev2 = vp_positions[vpNum-2] if vpNum-2 >= 0 else None
                vpPos_next2 = vp_positions[vpNum+2] if vpNum+2 < len(vp_positions) else None


                # skip conditional verb phrases (might, etc.)
                if set(vp_list).intersection(data_structures.VP_delete):
                    #print 'Conditional: ', vp_list
                    continue


                # simple negation detector
                if 'not' in vp_list or "n't" in vp_list:
                    continue

                #print vpNum, vp_list

                # search if vp has a match in the dictionary
                vp_ok = False
                passive_aux_verbs = []
                for pred in self.vocabulary.predicates:
                    predTokens = self.vocabulary.predicates_tokenized[pred]

                    # if the length of the word from dict is 1 this is simple
                    if len(predTokens) == 1:
                        if predTokens[0] in vp_list:
                            predicate = predTokens[0]

                            # check for auxiliary verbs of passive
                            #if pred in self.vocabulary.passive_all:
                            passive_aux_verbs = list(data_structures.PASSIVE_AUX_VERBS.intersection(vp_set))

                            vp_ok = True
                            break
                    # otherwise, check both cases: vp is longer or predicate is longer
                    else:
                        #TODO: tu so mozne izboljsave, ce upostevamo parsane predikate
                        # lahko bi uporabili tudi lematizacijo

                        predPositions = find_sublist_all(predTokens, sentTokens)
                        if predPositions == []:
                            continue

                        # check if the position of vp and some of the positions of predicate
                        # overlap in at least one token
                        pos_ok = False
                        for predPos in predPositions:
                            if len(predTokens) <= len(vp_list):
                                if predPos >= vpPos and (predPos < vpPos + len(vp_list)):
                                    pos_ok = True
                                    break
                            else:
                                if vpPos >= predPos and (vpPos < predPos + len(predTokens)):
                                    pos_ok = True
                                    break
                        #endfor
                        if pos_ok:
                            predicate = pred
                            vp_ok = True

                            # check for auxiliary verbs of passive
                            #if pred in self.vocabulary.passive_all:
                            passive_aux_verbs = list(data_structures.PASSIVE_AUX_VERBS.intersection(vp_set))

                            break
                #endfor
                if not vp_ok:
                    continue

                #print 'vp_list:', vp_list

                # now check all combinations of NPs
                for (i, np1) in enumerate(NPs):
                    # prevent searching impossible combinations (NP1 after VP)
                    if np1 > vp:
                        break

                    ## skip NP1s before the previous VP
                    if VP_CHECK_POS == 1:
                        if vpPos_prev and np_positions[i] < vpPos_prev:
                            continue
                    elif VP_CHECK_POS == 2:
                        if vpPos_prev2 and np_positions[i] < vpPos_prev2:
                            continue

                    np1_list = getPhrasesText_list(tree[np1])

                    # skip plant mutants etc.
                    if set(np1_list).intersection(data_structures.NP_delete):
                        #print 'Mutant: ', np1_list
                        continue


                    # simple negation detector
                    if 'nor' in np1_list or 'neither' in np1_list:
                        continue

                    #print 'np1:', np1_list

                    #!!!!!!!!!!!!!
                    #TODO: poisci vsa ujemanja (ce so), ne samo prvega!
                    # tako ujamemo npr. PR-1 and PR-5 induce ...
                    #!!!!!!!!!!!!

                    # check if the first NP matches the dictionary
                    #np1_ok = False
                    comp1_list = []
                    for (idx, comp) in enumerate(self.vocabulary.allCompounds_tokenized):
                        if find_sublist(comp, np1_list) != -1:
                            comp1 = self.vocabulary.allCompounds[idx]
                            comp1_list.append(comp1)
                            # if one compound covers the whole NP do not check for others as some shorter
                            # compound synonyms of other compounds may overlap
                            if len(comp) == len(np1_list):
                                break
                            #np1_ok = True
                            #break
                    if not comp1_list: #not np1_ok:
                        continue


                    triplet_found = False
                    for (j, np2) in enumerate(NPs[i+1:]):
                        # prevent searching impossible combinations (NP2 before VP)
                        if np2 < vp:
                            continue

                        # only check NP2s up to the next VP
                        #TODO: bolje: npr. max 2 NP oddaljen
                        #print 'vpPos_next, np_positions[i+j+1]', vpPos_next, np_positions[i+j+1]
                        if VP_CHECK_POS == 1:
                            if vpPos_next and np_positions[i+j+1] > vpPos_next:
                                break
                        elif VP_CHECK_POS == 2:
                            if vpPos_next2 and np_positions[i+j+1] > vpPos_next2:
                                break


                        # if one NP2 was found to be a part of a triplet, we move to the next NP1
                        # because using the next NP2 with the same NP1 is very likely an invalid triplet
                        # (the same NP1 and VP should not form more than one triplet in simple sentences)
                        #if triplet_found:
                            #break

                        np2_list = getPhrasesText_list(tree[np2]) # np2 kot seznam besed

                        # skip plant mutants etc.
                        if set(np2_list).intersection(data_structures.NP_delete):
                            #print 'Mutant: ', np2_list
                            continue

                        # simple negation detector
                        if 'nor' in np2_list or 'neither' in np2_list:
                            continue

                        #print 'np2:', np2_list

                        # check if the second NP matches the dictionary
                        #np2_ok = False
                        comp2_list = []
                        for (idx, comp) in enumerate(self.vocabulary.allCompounds_tokenized):
                            if find_sublist(comp, np2_list) != -1:
                                comp2 = self.vocabulary.allCompounds[idx]
                                comp2_list.append(comp2)
                                # if one compound covers the whole NP do not check for others as some shorter
                                # compound synonyms of other compounds may overlap
                                if len(comp) == len(np2_list):
                                    break
                                #np2_ok = True
                                #break
                        if not comp2_list: #not np2_ok:
                            continue

                        for (t, comp1) in enumerate(comp1_list):
                            for (u, comp2) in enumerate(comp2_list):
                                if self.vocabulary.compoundSynonyms[comp1] != self.vocabulary.compoundSynonyms[comp2]:
                                    triplet_found = True
                                    t = data_structures.Triplet(comp1,
                                                                predicate,
                                                                comp2,
                                                                document.rawSentences[snum],
                                                                document=document,
                                                                sentenceNumber=snum)

                                    # not all predicates from the dictionary are real passive
                                    # for those which are we store the auxiliary verbs
                                    t.passive_aux_verbs = passive_aux_verbs
                                    documentTriplets.append(t)

                        #if vp > np1 and vp < np2 and self.vocabulary.compoundSynonyms[comp1] != self.vocabulary.compoundSynonyms[comp2]:
                            #triplet_found = True
                            #t = data_structures.Triplet(comp1,
                                                        #predicate,
                                                        #comp2,
                                                        #document.rawSentences[snum],
                                                        #document=document,
                                                        #sentenceNumber=snum)

                            ## not all predicates from the dictionary are real passive
                            ## for those which are we store the auxiliary verbs
                            #t.passive_aux_verbs = passive_aux_verbs
                            #documentTriplets.append(t)

                            ## if one NP2 was found to be a part of triplet, we discard the rest of NP2s
                            ## as they are very likely too far away in the sentence to form correct
                            ## triplets with current VP and NP1
                            ##break

                    #endfor
                    #print
                #endfor
                #print
            #endfor
        #endfor

        return documentTriplets
        #return self.normalizeTriplets(documentTriplets)
    #end



#a = Vocabulary()
#a.loadCompounds_file('vocabulary/compounds.lst')
#a.loadPredicates_files(activationFname='vocabulary/activation.lst',
                          #inhibitionFname='vocabulary/inhibition.lst',
                          #bindingFname='vocabulary/binding.lst',
                          #activationFname_passive='vocabulary/activation_pas.lst',
                          #inhibitionFname_passive='vocabulary/inhibition_pas.lst',
                          #bindingFname_passive='vocabulary/binding_pas.lst',
                          #)
#te = TripletExtractor(v)

#doc = data_structures.Document('/mnt/wd2T/data/downloads/data/triplets/ripped/pmc2556844.txt')
#data_structures.SentenceSplitter().splitNLTK(doc)
#gtc = data_structures.GeniaTTC()
#gtc.process(doc)
##tsre = te.extractTripletsRE(doc)
#ts = te.extractTripletsNLP(doc)


#v = Vocabulary()
#v.loadCompounds_stringIO(StringIO.StringIO(open('vocabulary/compounds.lst').read()))
#v.loadPredicates_stringIO(activations=StringIO.StringIO(open('vocabulary/activation.lst').read()),
                          #inhibitions=StringIO.StringIO(open('vocabulary/inhibition.lst').read()),
                          #bindings=StringIO.StringIO(open('vocabulary/binding.lst').read()),
                          #activations_passive=StringIO.StringIO(open('vocabulary/activation_pas.lst').read()),
                          #inhibitions_passive=StringIO.StringIO(open('vocabulary/inhibition_pas.lst').read()),
                          #bindings_passive=StringIO.StringIO(open('vocabulary/binding_pas.lst').read())
                          #)
#a = TripletExtractor(v)



#doc = data_structures.Document('/mnt/wd2T/data/downloads/data/triplets/ripped/pmc2556844.txt')
#data_structures.SentenceSplitter().splitNLTK(doc)
#gtc = data_structures.GeniaTTC()
#gtc.process(doc)
##tsre = a.extractTripletsRE(doc)
#ts = a.extractTripletsNLP(doc)
