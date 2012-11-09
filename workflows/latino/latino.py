import string
import sys
import os
import pprint
import logging
import time

#------------------------------------------------------------------------------
# prepare environment for loading latino (Python.net interpreter should be used)
# see: http://pythonnet.sourceforge.net/
#------------------------------------------------------------------------------

sys.path.append('.\\workflows\\latino\\bin')

try:
    from LatinoCloudFlows import *
    import System
    import Latino
except Exception:
    pass
#    logging.warning("LatinoClowdFlows could not be imported! Either there are no Latino dll available or a "\
#                  "wrong interpreter is used. See 'http://pythonnet.sourceforge.net' for interpreter details. "\
#                  "Other functionality (besides latino) will work as .")

logging.basicConfig(format='%(asctime)s %(message)s', level=logging.INFO)

#------------------------------supposed------------------------------------------------
# Generic interfaces for pickling .net objects
#------------------------------------------------------------------------------
class SerializableObject:
    netObj = None
    constructor = '<constructor_func_name>'
    def __init__(self, object = None):
        self.netObj = object
        self.copyAttributes()
    def createInstance(self, dict):
        constr = eval(self.constructor)
        args = self.getConstructorArgs(dict)
        self.netObj = constr(*args)
        self.copyAttributes()
    def getConstructorArgs(self, dict):
        if dict.has_key('constructorArgs'):
            return tuple([val for key,val in dict['constructorArgs'].items()])
        return ()
    def copyAttributes(self): #copy all attributes from wrapped object to the wrapper
        pass
        #for attr in dir(self.netObj):
        #    if hasattr(self.netObj, attr) and not hasattr(self, attr):
        #        setattr(self, attr, getattr(self.netObj, attr))
    def __repr__(self):
        #return "<" + self.netObj.__str__() + " wrapped in "+self.__class__.__name__+">"
        return "<" + self.netObj.__str__() + ">"
    def __str__(self):
        return self.__repr__()
    def netObj(self):
        return self.netObj

class LatinoSerializableObject(SerializableObject):
    def __getstate__(self):
        logging.info('Serialize {0} with latino serializer (start)'.format(self.netObj.__class__.__name__))
        start = time.clock()
        byteData = LatinoCF.Save(self.netObj)
        elapsed = (time.clock() - start)
        logging.info('Serialize {0} with latino serializer (end, size: {1:,} chars) in {2} ms'.format(self.netObj.__class__.__name__, len(byteData),elapsed))
        return {'byteData': byteData}
    def __setstate__(self, dict):
        logging.info('Deserialize {0} with latino deserializer (start)'.format("<LatinoObject>"))
        start = time.clock()
        self.netObj = LatinoCF.Load(dict['byteData'])
        self.copyAttributes()
        elapsed = (time.clock() - start)
        logging.info('Deserialize {0} with latino deserializer (end, size: {1:,} chars) in {2} ms'.format(self.netObj.__class__.__name__, len(dict['byteData']),elapsed))
    def __repr__(self):
        return "<LSO: " + self.netObj.__str__() + ">"

LSO = LatinoSerializableObject

class ObjectFunctionSerializableObject(SerializableObject):
    save = '<save_data_func_name>'
    load = '<load_data_func_name>'
    def __getstate__(self):
        logging.info('Serialize {0} with obj.function {1} (start)'.format(self.__class__.__name__, self.save))
        saveFunc = getattr(self.netObj, self.save)
        data = saveFunc()
        logging.info('Serialize {0} with obj.function {1} (end, size: {2:,} chars)'.format(self.__class__.__name__, self.save, len(data)))
        return {'data': data}
    def __setstate__(self, dict):
        logging.info('Deserialize {0} with obj.function {1} (start)'.format(self.__class__.__name__, self.load))
        self.createInstance(dict)
        loadFunc = getattr(self.netObj, self.load)
        loadFunc(dict['data'])
        logging.info('Deserialize {0} with obj.function {1} (end, size: {2:,} chars)'.format(self.__class__.__name__, self.load, len(dict['data'])))

class ExternalFunctionSerializableObject(SerializableObject):
    save = '<save_data_func_name>'
    load = '<load_data_func_name>'
    def __getstate__(self):
        logging.info('Serialize {0} with ext.function {1} (start)'.format(self.__class__.__name__, self.save))
        saveFunc = eval(self.save)
        data = saveFunc(self.netObj)
        logging.info('Serialize {0} with ext.function {1} (end, size: {2:,} chars)'.format(self.__class__.__name__, self.save, len(data)))
        return {'data': data}
    def __setstate__(self, dict):
        logging.info('Deserialize {0} with ext.function {1} (start)'.format(self.__class__.__name__, self.load))
        loadFunc = eval(self.load)
        self.netObj = loadFunc(dict['data'])
        self.copyAttributes()
        logging.info('Deserialize {0} with ext.function {1} (end, size: {2:,} chars)'.format(self.__class__.__name__, self.load, len(dict['data'])))

class PropertySerializableObject(SerializableObject):
    constructorArgs = []
    props = []
    def __getstate__(self):
        logging.info('Serialize {0} properties (start)'.format(self.__class__.__name__))
        dictToSave = {'props':{},'constructorArgs':{}}
        for prop in self.props:
            if hasattr(self.netObj, prop):
                dictToSave['props'][prop] = getattr(self.netObj, prop)
        for constructorArg in self.constructorArgs:
            if hasattr(self.netObj, constructorArg):
                dictToSave['constructorArgs'][constructorArg] = getattr(self.netObj, constructorArg)
        logging.info('Serialize {0} properties (end {1:,} properties)'.format(self.__class__.__name__, len(dictToSave['props'])+len(dictToSave['constructorArgs'])))
        return dictToSave
    def __setstate__(self, dict):
        logging.info('Deserialize {0} properties (start)'.format(self.__class__.__name__))
        self.createInstance(dict)
        for prop in dict:
            if hasattr(self.netObj, prop):
                setattr(self.netObj, prop, dict[prop])
        logging.info('Deserialize {0} properties (end {1:,} properties)'.format(self.__class__.__name__, len(dict['props'])+len(dict['constructorArgs'])))

class ManuallySerializableObject(SerializableObject):
    def __getstate__(self):
        logging.info('Serialize {0} manually (start)'.format(self.__class__.__name__))
        dictToSave = self.getstate()
        logging.info('Serialize {0} manually ({1:,} properties)'.format(self.__class__.__name__, len(dictToSave)))
        return dictToSave
    def __setstate__(self, dict):
        logging.info('Deserialize {0} manually start)'.format(self.__class__.__name__))
        self.createInstance(dict)
        self.setstate(dict)
        logging.info('Deserialize {0} manually ({1:,} properties)'.format(self.__class__.__name__, len(dict)))
    def getstate(self):
        return {}
    def setstate(self, dict):
        pass

#------------------------------------------------------------------------------
# Actual interfaces for pickling .net objects
#------------------------------------------------------------------------------
#class PIDocumentCorpus(ExternalFunctionSerializableObject):
#    constructor = 'LatinoCF.ConstructDocumentCorpus'
#    save = 'LatinoCF.SaveADCtoXml'
#    load = 'LatinoCF.LoadADCfromXml'

#class PIEnglishMaximumEntropySentenceDetector(PropertySerializableObject):
#    constructor = 'LatinoCF.ConstructEnglishMaximumEntropySentenceDetector'
#    props = ['UnicodeMapping']

#class PIEnglishMaximumEntropyTokenizer(PropertySerializableObject):
#    constructor = 'LatinoCF.ConstructEnglishMaximumEntropyTokenizer'
#    props = ['UnicodeMapping', 'AlphaNumericOptimization']

#class PIUnicodeTokenizer(PropertySerializableObject):
#    constructor = 'LatinoCF.ConstructUnicodeTokenizer'
#    props = ['Filter', 'MinTokenLen']

#class PIEnglishMaximumEntropyPosTagger(PropertySerializableObject):
#    constructor = 'LatinoCF.ConstructEnglishMaximumEntropyPosTagger'
#    constructorArgs = ['BeamSize']
#    props = ['UnicodeMapping']

#class PILemmaSharpLemmatizer(LatinoSerializableObject):
#    constructor = 'LatinoCF.ConstructLemmaSharpLemmatizer'

#------------------------------------------------------------------------------
# Widget functions
#------------------------------------------------------------------------------

# Fibonacci example
def fib(n):
    fibSeq = LatinoCF.Fib(n)
    outList = list()
    for item in fibSeq:
        outList.append(item)
    return outList
def fibView(request, input_dict, output_dict, widget):
    newDict = input_dict.copy()
    newDict['enumIntegers'] = enumerate(input_dict['integers'])
    import django.shortcuts
    view = django.shortcuts.render(request, 'visualizations_latino/fib.html', {'widget': widget,
     'input_dict': newDict,
     'output_dict': output_dict})
    return view

# Loading and saving adc files
def loadADC(file, leadingLabels):
    return LSO(LatinoCF.LoadADC(file, ToBool(leadingLabels)))
def loadADCFromString(string, leadingLabels):
    return LSO(LatinoCF.LoadADCFromString(string, ToBool(leadingLabels)))
def getStrings(adc, elementAnnotation, elementFeatureConditions, delimiter, includeDocId):
    return ToPyObj(LatinoCF.GetDocStrings(adc.netObj, elementAnnotation, elementFeatureConditions, delimiter, ToBool(includeDocId)));
def ExtractDocumentsFeatures(adc, featureName):
    return ToPyObj(LatinoCF.ExtractDocumentsFeatures(ToNetObj(adc), featureName))
def AddDocumentsFeatures(adc, featureValues, featureName, featureValuePrefix):
    return ToPyObj(LatinoCF.AddDocumentsFeatures(ToNetObj(adc), ToNetObj(featureValues), featureName, featureValuePrefix))
def SplitDocumentsByFeatureValue(adc, featureCondition):
    return ToPyObj(LatinoCF.SplitDocumentsByFeatureValue(ToNetObj(adc), featureCondition))
def MarkDocumentsWithSetFeature(adc, featureName, featureValuePrefix, numOfSets, random, useSeed, randomSeed):
    _adc = ToNetObj(adc)
    _numOfSets = ToInt(numOfSets)
    _random = ToBool(random)
    _useSeed = ToBool(useSeed)
    _randomSeed = ToInt(randomSeed)
    adcNew = LatinoCF.MarkDocumentsWithSetFeature(_adc, featureName, featureValuePrefix, _numOfSets, _random, _useSeed, _randomSeed)
    return ToPyObj(adcNew)
def CorpusStatistics(adc):
    _adc = ToNetObj(adc)
    dict = LatinoCF.CorpusStatistics(_adc)
    return ToPyObj(dict)

# ADC conversion
def ADCtoXMLString(adc):
    return LatinoCF.SaveADCtoXml(adc.netObj)
def XMLStringtoADC(xml):
    return LSO(LatinoCF.LoadADCfromXml(xml))

# Text tokenization
def CreateEnglishMaximumEntropySentenceDetector(unicodeMapping):
    ss = LatinoCF.ConstructEnglishMaximumEntropySentenceDetector()
    ss.UnicodeMapping = ToBool(unicodeMapping)
    return LSO(ss)
def CreateEnglishMaximumEntropyTokenizer(unicodeMapping, alphaNumericOptimization):
    tok = LatinoCF.ConstructEnglishMaximumEntropyTokenizer()
    tok.UnicodeMapping = ToBool(unicodeMapping)
    tok.AlphaNumericOptimization = ToBool(alphaNumericOptimization)
    return LSO(tok)

def CreateUnicodeTokenizer(filter, minTokenLen):
    _filter = LatinoCF.ParseEnum[Latino.TextMining.TokenizerFilter](filter, Latino.TextMining.TokenizerFilter.None)
    _minTokenLen = ToInt(minTokenLen)
    tok = LatinoCF.ConstructUnicodeTokenizer(_filter, _minTokenLen)
    return LSO(tok)
def CreateSimpleTokenizer(type, minTokenLen):
    _type = LatinoCF.ParseEnum[Latino.TextMining.TokenizerType](type, Latino.TextMining.TokenizerType.AllChars)
    _minTokenLen = ToInt(minTokenLen)
    tok = LatinoCF.ConstructSimpleTokenizer(_type, _minTokenLen)
    return LSO(tok)
def CreateRegexTokenizer(tokenRegex, ignoreUnknownTokens, \
                         regexOptionsNone, regexOptionsIgnoreCase, regexOptionsMultiline, regexOptionsExplicitCapture,\
                         regexOptionsCompiled, regexOptionsSingleline, regexOptionsIgnorePatternWhitespace, regexOptionsRightToLeft,\
                         regexOptionsECMAScript, regexOptionsCultureInvariant):
    _tokenRegex = tokenRegex
    _ignoreUnknownTokens = ToBool(ignoreUnknownTokens)
    _regexOptions = 0;
    _regexOptions += (0   if ToBool(regexOptionsNone) else 0)
    _regexOptions += (1   if ToBool(regexOptionsIgnoreCase) else 0)
    _regexOptions += (2   if ToBool(regexOptionsMultiline) else 0)
    _regexOptions += (4   if ToBool(regexOptionsExplicitCapture) else 0)
    _regexOptions += (8   if ToBool(regexOptionsCompiled) else 0)
    _regexOptions += (16  if ToBool(regexOptionsSingleline) else 0)
    _regexOptions += (32  if ToBool(regexOptionsIgnorePatternWhitespace) else 0)
    _regexOptions += (64  if ToBool(regexOptionsRightToLeft) else 0)
    _regexOptions += (256 if ToBool(regexOptionsECMAScript) else 0)
    _regexOptions += (512 if ToBool(regexOptionsCultureInvariant) else 0)
    tok = LatinoCF.ConstructRegexTokenizer(_tokenRegex, _ignoreUnknownTokens, _regexOptions)
    return LSO(tok)
def Tokenize(adc, tokenizer, inputAnnotation, outputAnnotation):
    dc = LatinoCF.Tokenize(adc.netObj, tokenizer.netObj, inputAnnotation, outputAnnotation)
    return LSO(dc)
def TokenizeString(string, tokenizer):
    netObj = ToNetObj(string)
    tokObj = LatinoCF.TokenizeString(netObj, tokenizer.netObj)
    dc = ToPyObj(tokObj)
    return dc
def TokenizeMultiple(adc, tokenizer, inputAnnotation, outputAnnotation):
    dc = adc.netObj
    inputAnnotations = inputAnnotation.splitlines(False)
    outputAnnotations = outputAnnotation.splitlines(False)
    tokenizerCount = min(len(tokenizer), len(inputAnnotations), len(outputAnnotations))
    if tokenizerCount!=len(tokenizer) or tokenizerCount!=len(inputAnnotations) or tokenizerCount!=len(outputAnnotations):
        raise Exception("Number of tokenizers must mach number of input annotations and number of input annotations defined in widget properties!")
    for i in range(tokenizerCount):
        dc = LatinoCF.Tokenize(dc, tokenizer[i].netObj, inputAnnotations[i].strip(), outputAnnotations[i].strip())
    return LSO(dc)

# POS Tagging
def CreateEnglishMaximumEntropyPosTagger(beamSize, unicodeMapping):
    pt = LatinoCF.ConstructEnglishMaximumEntropyPosTagger(beamSize)
    pt.BeamSize = ToInt(beamSize)
    pt.UnicodeMapping = ToBool(unicodeMapping)
    return ToPyObj(pt)
def PosTag(adc, posTagger, groupAnnotation, elementAnnotation, outputFeature):
    dc = LatinoCF.PosTag(ToNetObj(adc), ToNetObj(posTagger), groupAnnotation, elementAnnotation, outputFeature)
    return ToPyObj(dc)
def PosTagString(string, posTagger, outputFeature):
    netObj = ToNetObj(string)
    tokObj = LatinoCF.PosTagString(netObj, posTagger.netObj, outputFeature)
    dc = ToPyObj(tokObj)
    return dc

# Tagging
def ConstructLemmaSharpLemmatizer(language):
    lang = LatinoCF.ParseEnum[Latino.TextMining.Language](language, Latino.TextMining.Language.English)
    ls = LatinoCF.ConstructLemmaSharpLemmatizer(lang)
    return LSO(ls)
def ConstructSnowballStemmer(language):
    lang = LatinoCF.ParseEnum[Latino.TextMining.Language](language, Latino.TextMining.Language.English)
    ls = LatinoCF.ConstructSnowballStemmer(lang)
    return LSO(ls)
def ConstructStopWordsTagger(stopWords, ignoreCase):
    sw = stopWords
    if IsSequence(stopWords):
        sw =  "".join([x+"\n" for x in Flatten(stopWords)])
    swt = LatinoCF.ConstructStopWordsTagger(sw, ignoreCase)
    return LSO(swt)
def ConstructConditionTagger(featureCondition, outputFeatureValue, elementsTextToFeatureValue):
    ct = LatinoCF.ConstructConditionTagger(featureCondition, outputFeatureValue, elementsTextToFeatureValue)
    return LSO(ct)
def TagString(string, tagger, outputFeature):
    netObj = ToNetObj(string)
    tokObj = LatinoCF.TagString(netObj, tagger.netObj, outputFeature)
    dc = ToPyObj(tokObj)
    return dc
def TagADC(adc, tagger, elementAnnotation, outputFeature):
    dc = LatinoCF.TagADC(adc.netObj, tagger.netObj, elementAnnotation, outputFeature)
    return LSO(dc)
def TagADCMultiple(adc, tagger, elementAnnotation, outputFeature):
    dc = adc.netObj
    elementAnnotations = elementAnnotation.splitlines(False)
    outputFeatures = outputFeature.splitlines(False)
    taggerCount = min(len(tagger), len(elementAnnotations), len(outputFeatures))
    if taggerCount!=len(tagger) or taggerCount!=len(elementAnnotations) or taggerCount!=len(outputFeatures):
        raise Exception("Number of taggers must mach number of element annotations and number of feature names defined in widget properties!")
    for i in range(taggerCount):
        dc = LatinoCF.TagADC(dc, tagger[i].netObj, elementAnnotations[i].strip(), outputFeatures[i].strip())
    return LSO(dc)

# Tagging helper functions
def GetStopWords(language):
    lang = LatinoCF.ParseEnum[Latino.TextMining.Language](language, Latino.TextMining.Language.English)
    sw = LatinoCF.GetStopWords(lang)
    return ToPyList(sw)

# Bow creation and manipulation
def ConstructBowSpaceFromTexts(documents, tokenizer, stemmer, stopwords, maxNGramLen, minWordFreq, wordWeightType, cutLowWeightsPerc, normalizeVectors):
    _documents = ToNetObj(documents)
    _tokenizer = ToNetObj(tokenizer)
    _stemmer = ToNetObj(stemmer)
    _stopwords = ToNetObj(stopwords)
    _maxNGramLen = ToInt(maxNGramLen)
    _minWordFreq = ToInt(minWordFreq)
    _wordWeightType = ToEnum(Latino.TextMining.WordWeightType, wordWeightType, Latino.TextMining.WordWeightType.TfIdf)
    _cutLowWeightsPerc = ToFloat(cutLowWeightsPerc)
    _normalizeVectors = ToBool(normalizeVectors)
    bow = LatinoCF.ConstructBowSpace(_documents, _tokenizer, _stemmer, _stopwords, _maxNGramLen, _minWordFreq, _wordWeightType, _cutLowWeightsPerc, _normalizeVectors)
    return ToPyObj(bow)
def ConstructBowSpaceFromADC(adc, tokenId, stemId, stopwordId, labelId, maxNGramLen, minWordFreq, wordWeightType, cutLowWeightsPerc, normalizeVectors):
    _adc = ToNetObj(adc)
    _maxNGramLen = ToInt(maxNGramLen)
    _minWordFreq = ToInt(minWordFreq)
    _wordWeightType = ToEnum(Latino.TextMining.WordWeightType, wordWeightType, Latino.TextMining.WordWeightType.TfIdf)
    _cutLowWeightsPerc = ToFloat(cutLowWeightsPerc)
    _normalizeVectors = ToBool(normalizeVectors)
    bow = LatinoCF.ConstructBowSpace(_adc, tokenId, stemId, stopwordId, labelId, _maxNGramLen, _minWordFreq, _wordWeightType, _cutLowWeightsPerc, _normalizeVectors)
    return ToPyObj(bow)

def GetVocabulary(bow, startIndex, maxWords):
    _bow = ToNetObj(bow)
    _startIndex = ToInt(startIndex)
    _maxWords = ToInt(maxWords)
    vocab = LatinoCF.GetVocabulary(_bow, _startIndex, _maxWords)
    return ToPyObj(vocab)
def GetDocumentVectors(bow):
    ds = LatinoCF.GetDocumentVectors( ToNetObj(bow))
    return ToPyObj(ds)
def ProcessNewDocuments(bow, obj):
    ds = LatinoCF.ProcessNewDocuments(ToNetObj(bow), ToNetObj(obj))
    return ToPyObj(ds)

# Document vectors manipulation
def AddLabelsToDocumentVectors(ds, labels):
    ldv = LatinoCF.AddLabelsToDocumentVectors(ToNetObj(ds), ToNetObj(labels))
    return ToPyObj(ldv)
def RemoveDocumentVectorsLabels(ds):
    ds = LatinoCF.RemoveDocumentVectorsLabels(ToNetObj(ds))
    return ToPyObj(ds)
def ExtractDatasetLabels(ds):
    labels = LatinoCF.ExtractDatasetLabels(ToNetObj(ds))
    return ToPyObj(labels)
def DatasetSplitSimple(input_dict):
    _ds = ToNetObj(input_dict['ds'])
    _percentage = ToFloat(input_dict['percentage'])
    _randomSeed = ToInt(input_dict['randomSeed'])
    dict = LatinoCF.DatasetSplitSimple(_ds, _percentage, _randomSeed)
    return ToPyObj(dict)

def CalculateSimilarityMatrix(dv1,dv2, thresh, fullMatrix):
    matrix = LatinoCF.CalculateSimilarityMatrix(ToNetObj(dv1), ToNetObj(dv2), ToFloat(thresh), ToBool(fullMatrix))
    return ToPyObj(matrix)
def SparseMatrixToTable(smx):
    table = LatinoCF.SparseMatrixToTable(ToNetObj(smx))
    return ToPyObj(table)

# Clustering
def ConstructKMeansClusterer(k, centroidType, similarityModel, randomSeed, eps, trials):
    _k = ToInt(k)
    _centroidType = ToEnum(Latino.Model.CentroidType, centroidType, Latino.Model.CentroidType.NrmL2)
    _similarityModel =  ToEnum(SimilarityModel, similarityModel, SimilarityModel.Cosine)
    _randomSeed = ToInt(randomSeed)
    _eps = ToFloat(eps)
    _trials = ToInt(trials)
    clusterer = LatinoCF.ConstructKMeansClusterer(_k, _centroidType, _similarityModel, _randomSeed, _eps, _trials)
    return ToPyObj(clusterer)
def ConstructKMeansFastClusterer(k, randomSeed, eps, trials):
    _k = ToInt(k)
    _randomSeed = ToInt(randomSeed)
    _eps = ToFloat(eps)
    _trials = ToInt(trials)
    clusterer = LatinoCF.ConstructKMeansFastClusterer(_k, _randomSeed, _eps, _trials)
    return ToPyObj(clusterer)
def ConstructHierarchicalBisectingClusterer(minQuality):
    clusterer = LatinoCF.ConstructHierarchicalBisectingClusterer(ToFloat(minQuality))
    return ToPyObj(clusterer)

def ClusterDocumentVectors(clusterer, ds):
    clustRes = LatinoCF.ClusterDocumentVectors(ToNetObj(clusterer), ToNetObj(ds))
    return ToPyObj(clustRes)
def ClusteringResultsInfo(clustRes):
    clustResInfo = LatinoCF.ClusteringResultsInfo(ToNetObj(clustRes))
    return ToPyObj(clustResInfo)

# Classification
def ConstructCentroidClassifier(similarityModel, normalizeCentorids):
    _similarityModel =  ToEnum(SimilarityModel, similarityModel, SimilarityModel.Cosine)
    _normalizeCentorids = ToBool(normalizeCentorids)
    classifier = LatinoCF.ConstructCentroidClassifier(_similarityModel, _normalizeCentorids)
    return ToPyObj(classifier)

def ConstructCentroidClassifier(input_dict):
    _similarityModel = ToEnum(SimilarityModel, input_dict['similarityModel'], SimilarityModel.Cosine)
    _normalizeCentorids = ToBool(input_dict['normalizeCentorids'])
    classifier = LatinoCF.ConstructCentroidClassifier(_similarityModel, _normalizeCentorids)
    return {'classifier': ToPyObj(classifier)}
def ConstructNaiveBayesClassifier(input_dict):
    _normalize = ToBool(input_dict['normalize'])
    _logSumExpTrick = ToBool(input_dict['logSumExpTrick'])
    classifier = LatinoCF.ConstructNaiveBayesClassifier(_normalize, _logSumExpTrick)
    return {'classifier': ToPyObj(classifier)}
def ConstructSvmBinaryClassifier(input_dict):
    _c = ToFloat(input_dict['c'])
    _biasedHyperplane = ToBool(input_dict['biasedHyperplane'])
    _kernelType = ToEnum(Latino.Model.SvmLightKernelType, input_dict['kernelType'], Latino.Model.SvmLightKernelType.Linear)
    _kernelParamGamma = ToFloat(input_dict['kernelParamGamma'])
    _kernelParamD = ToFloat(input_dict['kernelParamD'])
    _kernelParamS = ToFloat(input_dict['kernelParamS'])
    _kernelParamC = ToFloat(input_dict['kernelParamC'])
    _eps = ToFloat(input_dict['eps'])
    _maxIter = ToInt(input_dict['maxIter'])
    _customParams = ToString(input_dict['customParams'])
    classifier = LatinoCF.ConstructSvmBinaryClassifier(_c, _biasedHyperplane, _kernelType, _kernelParamGamma, _kernelParamD, _kernelParamS, _kernelParamC, _eps, _maxIter, _customParams)
    return {'classifier': ToPyObj(classifier)}
def ConstructSvmMulticlassFast(input_dict):
    _c = ToFloat(input_dict['c'])
    _eps = ToFloat(input_dict['eps'])
    classifier = LatinoCF.ConstructSvmMulticlassFast(_c, _eps)
    return {'classifier': ToPyObj(classifier)}
def ConstructMajorityClassifier(input_dict):
    classifier = LatinoCF.ConstructMajorityClassifier()
    return {'classifier': ToPyObj(classifier)}
def ConstructMaximumEntropyClassifier(input_dict):
    _moveData = ToBool(input_dict['moveData'])
    _numIter = ToInt(input_dict['numIter'])
    _cutOff = ToInt(input_dict['cutOff'])
    _numThreads = ToInt(input_dict['numThreads'])
    _normalize = ToBool(input_dict['normalize'])
    classifier = LatinoCF.ConstructMaximumEntropyClassifier(_moveData, _numIter, _cutOff, _numThreads, _normalize)
    return {'classifier': ToPyObj(classifier)}
def ConstructMaximumEntropyClassifierFast(input_dict):
    _moveData = ToBool(input_dict['moveData'])
    _numIter = ToInt(input_dict['numIter'])
    _cutOff = ToInt(input_dict['cutOff'])
    _numThreads = ToInt(input_dict['numThreads'])
    _normalize = ToBool(input_dict['normalize'])
    classifier = LatinoCF.ConstructMaximumEntropyClassifierFast(_moveData, _numIter, _cutOff, _numThreads, _normalize)
    return {'classifier': ToPyObj(classifier)}
def ConstructKnnClassifier(input_dict):
    _similarityModel = ToEnum(SimilarityModel, input_dict['similarityModel'], SimilarityModel.Cosine)
    _k = ToInt(input_dict['k'])
    _softVoting = ToBool(input_dict['softVoting'])
    classifier = LatinoCF.ConstructKnnClassifier(_similarityModel, _k, _softVoting)
    return {'classifier': ToPyObj(classifier)}
def ConstructKnnClassifierFast(input_dict):
    _k = ToInt(input_dict['k'])
    _softVoting = ToBool(input_dict['softVoting'])
    classifier = LatinoCF.ConstructKnnClassifierFast(_k, _softVoting)
    return {'classifier': ToPyObj(classifier)}

def TrainClassifier(csf, ds):
    classifier = LatinoCF.TrainClassifier(ToNetObj(csf), ToNetObj(ds))
    return ToPyObj(classifier)

def PredictClassification(csf, ds):
    predictions = LatinoCF.PredictClassification(ToNetObj(csf), ToNetObj(ds))
    return ToPyObj(predictions)

def PredictionToLabel(predictions):
    labels = LatinoCF.PredictionToLabel(ToNetObj(predictions))
    return ToPyObj(labels)
def PredictionInfo(predictions):
    predictInfos = LatinoCF.PredictionInfo(ToNetObj(predictions))
    return ToPyObj(predictInfos)

def TrainClassifier(csf, ds):
    classifier = LatinoCF.TrainClassifier(ToNetObj(csf), ToNetObj(ds))
    return ToPyObj(classifier)

def CrossValidation(csf, ds, numOfSets, random, useSeed, randomSeed):
    obj = LatinoCF.CrossValidation(ToNetObj(csf), ToNetObj(ds), ToInt(numOfSets), ToBool(random), ToBool(useSeed), ToInt(randomSeed))
    return ToPyObj(obj)
def CrossValidationPredefSplits(csf, ds, sets):
    obj = LatinoCF.CrossValidationPredefSplits(ToNetObj(csf), ToNetObj(ds), ToNetObj(ToIntList(sets)))
    pyObj = ToPyObj(obj)
    return pyObj

#------------------------------------------------------------------------------
# Visualisation functions
#------------------------------------------------------------------------------

# ADC visualisation
def adcView(request, input_dict, output_dict, widget):
    import django.shortcuts
    view = django.shortcuts.render(request, 'visualizations/adc.html', {'widget': widget,
                                                                               'input_dict': input_dict,
                                                                               'output_dict': output_dict})
    return view
def makeAdcIndexPage(adc, document_id_from, document_id_to, narrow_doc):
    from django.http import HttpResponse
    data = adc.netObj.MakeIndexPage(document_id_from, document_id_to, 100, narrow_doc)
    return HttpResponse(data, mimetype='text/html')
def makeAdcDocPage(adc, docId, narrow_doc):
    from django.http import HttpResponse
    data = adc.netObj.MakeDocumentPage(docId, narrow_doc)
    return HttpResponse(data, mimetype='text/html')

def ShowTable(request, input_dict, output_dict, widget):
    import django.shortcuts
    tbl= input_dict['tbl']
    output_dict = {'attrs':list(tbl.items()[0][0]), 'metas':[], 'data':tbl.values()[0], 'title':'Vocabulary Table'}
    templ = django.template.loader.get_template('visualizations/table_viewer.html')
    view = django.shortcuts.render(request, 'visualizations/table_viewer.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
    return view

#------------------------------------------------------------------------------
# Helper functions
#------------------------------------------------------------------------------
def ToInt(s):
    try:
        return int(s)
    except:
        return 0
def ToFloat(s):
    try:
        return float(s)
    except:
        return 0
def ToBool(s):
    if s == 'true' or s == 'True':
        return True
    else:
        return False
def ToString(s):
    return s
def ToEnum(typ, s,defaultValue):
    return LatinoCF.ParseEnum[typ](s, defaultValue)
def ToIntList(s):
    il = []
    for i in s:
        il.append(ToInt(i))
    return il
def ToPyList(l):
    return [x for x in l]
def ToNetList(Type,l):
    a = System.Array[Type](l)
    return a
def ToNetObj(data):
    if hasattr(data, "netObj"):
        return data.netObj
    if isinstance(data, dict):
        if not len(data):
            return System.Collections.Generic.Dictionary[System.Object,System.Object]()
        else:
            key = ToNetObj(data.keys()[0])
            val = ToNetObj(data.values()[0])
            for tryIndex in [0, 1]:
                try:
                    if not tryIndex:
                        d = System.Collections.Generic.Dictionary[type(key),type(val)]()
                    else:
                        d = System.Collections.Generic.Dictionary[System.Object,System.Object]()
                    for key,val in data.items():
                        k = ToNetObj(key)
                        v = ToNetObj(val)
                        d[k] = v
                    return d
                except:
                    pass
            return System.Object()
    if isinstance(data, list):
        if not len(data):
            return System.Collections.Generic.List[System.Object]()
        else:
            dataNet = ToNetObj(data[0])
            for tryIndex in [0, 1]:
                try:
                    if not tryIndex:
                        l = System.Collections.Generic.List[type(dataNet)]()
                    else:
                        l = System.Collections.Generic.List[System.Object]()
                    for i, val in enumerate(data):
                        l.Add(ToNetObj(val))
                    return l
                except:
                    pass
            return System.Object()
    if isinstance(data, tuple):
        if not len(data):
            return System.Collections.Generic.LinkedList[System.Object]()
        else:
            dataNet = ToNetObj(data[0])
            for tryIndex in [0, 1]:
                try:
                    if not tryIndex:
                        a = System.Collections.Generic.LinkedList[type(dataNet)]()
                    else:
                        a = System.Collections.Generic.LinkedList[System.Object]()
                    for i, val in enumerate(data):
                        a.AddLast(ToNetObj(val))
                    return a
                except:
                    pass
            return System.Object()
    return data
def ToPyObj(data):
    if hasattr(data, "GetType") and data.GetType().IsGenericType:
        genType = data.GetType().GetGenericTypeDefinition()
        if genType.Equals(System.Collections.Generic.Dictionary):
            d = {}
            for keyVal in data:
                k = ToPyObj(keyVal.Key)
                v = ToPyObj(keyVal.Value)
                d[k] = v
            return d
        if genType.Equals(System.Collections.Generic.List):
            l = []
            for val in data:
                l.append(ToPyObj(val))
            return l
        if genType.Equals(System.Collections.Generic.LinkedList):
            l = []
            for val in data:
                l.append(ToPyObj(val))
            return tuple(l)
    if hasattr(data, "GetType"):
        for interface in data.GetType().GetInterfaces():
            if interface.Name == u'ISerializable':
                return LSO(data)
    return data
def IsSequence(arg):
    return (not hasattr(arg, "strip") and
            hasattr(arg, "__getitem__") or
            hasattr(arg, "__iter__"))
def Flatten(l, ltypes=(list, tuple)):
    ltype = type(l)
    l = list(l)
    i = 0
    while i < len(l):
        while isinstance(l[i], ltypes):
            if not l[i]:
                l.pop(i)
                i -= 1
                break
            else:
                l[i:i + 1] = l[i]
        i += 1
    return ltype(l)

#------------------------------------------------------------------------------
# SUPPLEMENTARY FUNCTIONS
#------------------------------------------------------------------------------

def RunCSharpSnippet(snippetParams, snippetCode, aditionalReferences, usings):
    snippetParams.append(System.Object()) # add dummy object so list is converted to list of objects and not list of any other types
    results = LatinoCF.RunCSharpSnippet(ToNetObj(snippetParams), snippetCode, aditionalReferences, usings)
    return { 'code' : ToPyObj(results.GeneratedCode), 'error' : results.ErrorString, 'out' : ToPyObj(results.Output), 'consoleOut' : results.ConsoleOutput}
def FlattenObjectToStringArray(data):
    dataNet = ToNetObj(data)
    flatList = LatinoCF.FlattenObjectToStringArray(dataNet)
    return ToPyObj(flatList)

#------------------------------------------------------------------------------
# Playground
#------------------------------------------------------------------------------
#a = ConstructLemmaSharpLemmatizer("English")
#print RunCSharpSnippet([a,5], "_out = ((List<object>)_in)[1];", "", "using System; using System.Collections.Generic;")

#//            string usings = ;
#//            string innerCode = @"
#//                for (int i=0;i<10;i++)
#//                    Console.WriteLine(String.Format(""InnerWrite{0}"",i));
#//                return 23;
#//                ";
#//            string[] aditionalReferences = null;
#//            object[] snippetParams = new object[]{};

#print type(Latino.ISerializable)
#a = ConstructLemmaSharpLemmatizer("English")
#print ToNetObj(({a:a},[a]))
#print ToPyObj(ToNetObj(({a:a},[a])))
#data = {1:(["Asdasdasd asdas asdas dasd","asdasdasd asdas asdas dasd"],"matjaz, je , hecen"), 2:3, "heca ni":"hec je", 4:{}}

#tok = LatinoCF.ConstructUnicodeTokenizer(Latino.TextMining.TokenizerFilter.None, 2)
#tok = LatinoCF.ConstructSimpleTokenizer(Latino.TextMining.TokenizerType.AllChars, 1)
#tok = LatinoCF.ConstructEnglishMaximumEntropyTokenizer()
#a = LatinoCF.TokenizeString(ToNetObj(data), tok)

#pprint.pprint(data)
#pprint.pprint(ToPyObj(a))
#print ToPyObj(a)

#import System

#a = LatinoCF.ConstructStopWordsTagger(["erkelr","asdsad"], True)
#logging.info(a)
#logging.info(a.GetTag("asdsad"))
#
#b = LatinoSerializableObject(a)
#
#logging.info(b.netObj.StopWords)
#
#import cPickle
#c = cPickle.dumps(b);
#d = cPickle.loads(c)
#logging.info(d.netObj.StopWords)
#logging.info(a)
#logging.info(b)
#logging.info(c)
#logging.info(d)
#
#import objectPrint.info(as opp
#
#logging.info(dir(b.netObj.StopWords))
#logging.info(dir(b.netObj))
#
#logging.info(opp.ppprint(b.netObj))






