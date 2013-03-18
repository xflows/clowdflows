# -----------------------------------------------------------------------------------------------------
# WARNING: THIS IS AUTOMATICALLY GENERATED FILE, DO NOT EDIT IT MANUALLY AS YOU MAY LOOSE YOUR CHANGES!
# -----------------------------------------------------------------------------------------------------

from import_dotnet import *
from serialization_utils import *

def latino_flatten_object_to_string_array(inputDict):
    _data = ToNetObj(inputDict['data'])
    execResult = LatinoCF.FlattenObjectToStringArray(_data)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['flatData'] = execResultPy
    return outputDict

def latino_load_adc(inputDict):
    _file = ToString(inputDict['file'])
    _tabSeparatedTitle = ToBool(inputDict['tabSeparatedTitle'])
    _leadingLabels = ToBool(inputDict['leadingLabels'])
    execResult = LatinoCF.LoadADC(_file, _tabSeparatedTitle, _leadingLabels)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_load_adcfrom_string(inputDict):
    _plainString = ToString(inputDict['plainString'])
    _tabSeparatedTitle = ToBool(inputDict['tabSeparatedTitle'])
    _leadingLabels = ToBool(inputDict['leadingLabels'])
    execResult = LatinoCF.LoadADCFromString(_plainString, _tabSeparatedTitle, _leadingLabels)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_save_adcto_xml(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    execResult = LatinoCF.SaveADCtoXml(_adc)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['string'] = execResultPy
    return outputDict

def latino_load_adcfrom_xml(inputDict):
    _xml = ToString(inputDict['xml'])
    execResult = LatinoCF.LoadADCfromXml(_xml)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_get_doc_strings(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _elementAnnotation = ToString(inputDict['elementAnnotation'])
    _elementFeatureConditions = ToString(inputDict['elementFeatureConditions'])
    _delimiter = ToString(inputDict['delimiter'])
    _includeDocId = ToBool(inputDict['includeDocId'])
    execResult = LatinoCF.GetDocStrings(_adc, _elementAnnotation, _elementFeatureConditions, _delimiter, _includeDocId)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['strings'] = execResultPy
    return outputDict

def latino_extract_documents_features(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _featureName = ToString(inputDict['featureName'])
    execResult = LatinoCF.ExtractDocumentsFeatures(_adc, _featureName)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['strings'] = execResultPy
    return outputDict

def latino_add_documents_features(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _featureValues = ToNetObj(inputDict['featureValues'])
    _featureName = ToString(inputDict['featureName'])
    _featureValuePrefix = ToString(inputDict['featureValuePrefix'])
    execResult = LatinoCF.AddDocumentsFeatures(_adc, _featureValues, _featureName, _featureValuePrefix)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_add_computed_features(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _featureName = ToString(inputDict['featureName'])
    _featureComputataion = ToString(inputDict['featureComputataion'])
    _featureSpec = ToString(inputDict['featureSpec'])
    execResult = LatinoCF.AddComputedFeatures(_adc, _featureName, _featureComputataion, _featureSpec)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_split_documents_by_feature_value(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _featureCondition = ToString(inputDict['featureCondition'])
    _discardFilteredOut = ToBool(inputDict['discardFilteredOut'])
    execResult = LatinoCF.SplitDocumentsByFeatureValue(_adc, _featureCondition, _discardFilteredOut)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adcFiltered'] = execResultPy['adcFiltered']
    outputDict['adcRest'] = execResultPy['adcRest']
    return outputDict

def latino_extract_documents(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _indexList = ToNetObj(inputDict['indexList'])
    _discardFilteredOut = ToBool(inputDict['discardFilteredOut'])
    execResult = LatinoCF.ExtractDocuments(_adc, _indexList, _discardFilteredOut)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adcFiltered'] = execResultPy['adcFiltered']
    outputDict['adcRest'] = execResultPy['adcRest']
    return outputDict

def latino_mark_documents_with_set_feature(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _featureName = ToString(inputDict['featureName'])
    _featureValuePrefix = ToString(inputDict['featureValuePrefix'])
    _numOfSets = ToInt(inputDict['numOfSets'])
    _random = ToBool(inputDict['random'])
    _useSeed = ToBool(inputDict['useSeed'])
    _randomSeed = ToInt(inputDict['randomSeed'])
    execResult = LatinoCF.MarkDocumentsWithSetFeature(_adc, _featureName, _featureValuePrefix, _numOfSets, _random, _useSeed, _randomSeed)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_corpus_statistics(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    execResult = LatinoCF.CorpusStatistics(_adc)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['docCount'] = execResultPy['docCount']
    outputDict['featureCount'] = execResultPy['featureCount']
    outputDict['description'] = execResultPy['description']
    return outputDict

def latino_random_cross_validation_sets(inputDict):
    _numOfSets = ToInt(inputDict['numOfSets'])
    _numOfExamples = ToInt(inputDict['numOfExamples'])
    _random = ToBool(inputDict['random'])
    _useSeed = ToBool(inputDict['useSeed'])
    _randomSeed = ToInt(inputDict['randomSeed'])
    _examples = ToNetObj(inputDict['examples'])
    execResult = LatinoCF.RandomCrossValidationSets(_numOfSets, _numOfExamples, _random, _useSeed, _randomSeed, _examples)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['exampleSetId'] = execResultPy
    return outputDict

def latino_random_sequential_validation_sets(inputDict):
    _numOfSets = ToInt(inputDict['numOfSets'])
    _numOfExamples = ToInt(inputDict['numOfExamples'])
    _random = ToBool(inputDict['random'])
    _useSeed = ToBool(inputDict['useSeed'])
    _randomSeed = ToInt(inputDict['randomSeed'])
    _trainSize = ToString(inputDict['trainSize'])
    _testSize = ToString(inputDict['testSize'])
    _trainTestDelay = ToString(inputDict['trainTestDelay'])
    _examples = ToNetObj(inputDict['examples'])
    execResult = LatinoCF.RandomSequentialValidationSets(_numOfSets, _numOfExamples, _random, _useSeed, _randomSeed, _trainSize, _testSize, _trainTestDelay, _examples)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['dict'] = execResultPy
    return outputDict

def latino_get_multi_set_indexes(inputDict):
    _sets = ToNetObj(inputDict['sets'])
    execResult = LatinoCF.GetMultiSetIndexes(_sets)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['dict'] = execResultPy
    return outputDict

def latino_construct_english_maximum_entropy_sentence_detector(inputDict):
    execResult = LatinoCF.ConstructEnglishMaximumEntropySentenceDetector()
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['tokenizer'] = execResultPy
    return outputDict

def latino_construct_english_maximum_entropy_tokenizer(inputDict):
    _alphaNumericOptimization = ToBool(inputDict['alphaNumericOptimization'])
    execResult = LatinoCF.ConstructEnglishMaximumEntropyTokenizer(_alphaNumericOptimization)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['tokenizer'] = execResultPy
    return outputDict

def latino_construct_unicode_tokenizer(inputDict):
    _filter = ToEnum(Latino.TextMining.TokenizerFilter, inputDict['filter'], Latino.TextMining.TokenizerFilter.None)
    _minTokenLen = ToInt(inputDict['minTokenLen'])
    execResult = LatinoCF.ConstructUnicodeTokenizer(_filter, _minTokenLen)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['tokenizer'] = execResultPy
    return outputDict

def latino_construct_simple_tokenizer(inputDict):
    _type = ToEnum(Latino.TextMining.TokenizerType, inputDict['type'], Latino.TextMining.TokenizerType.AllChars)
    _minTokenLen = ToInt(inputDict['minTokenLen'])
    execResult = LatinoCF.ConstructSimpleTokenizer(_type, _minTokenLen)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['tokenizer'] = execResultPy
    return outputDict

def latino_construct_regex_tokenizer(inputDict):
    _tokenRegex = ToString(inputDict['tokenRegex'])
    _ignoreUnknownTokens = ToBool(inputDict['ignoreUnknownTokens'])
    _regexOptionsIgnoreCase = ToBool(inputDict['regexOptionsIgnoreCase'])
    _regexOptionsMultiline = ToBool(inputDict['regexOptionsMultiline'])
    _regexOptionsExplicitCapture = ToBool(inputDict['regexOptionsExplicitCapture'])
    _regexOptionsCompiled = ToBool(inputDict['regexOptionsCompiled'])
    _regexOptionsSingleline = ToBool(inputDict['regexOptionsSingleline'])
    _regexOptionsIgnorePatternWhitespace = ToBool(inputDict['regexOptionsIgnorePatternWhitespace'])
    _regexOptionsRightToLeft = ToBool(inputDict['regexOptionsRightToLeft'])
    _regexOptionsECMAScript = ToBool(inputDict['regexOptionsECMAScript'])
    _regexOptionsCultureInvariant = ToBool(inputDict['regexOptionsCultureInvariant'])
    execResult = LatinoCF.ConstructRegexTokenizer(_tokenRegex, _ignoreUnknownTokens, _regexOptionsIgnoreCase, _regexOptionsMultiline, _regexOptionsExplicitCapture, _regexOptionsCompiled, _regexOptionsSingleline, _regexOptionsIgnorePatternWhitespace, _regexOptionsRightToLeft, _regexOptionsECMAScript, _regexOptionsCultureInvariant)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['tokenizer'] = execResultPy
    return outputDict

def latino_tokenize_sentences(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _tokenizer = ToNetObj(inputDict['tokenizer'])
    _inputAnnotation = ToString(inputDict['inputAnnotation'])
    _outputAnnotation = ToString(inputDict['outputAnnotation'])
    execResult = LatinoCF.TokenizeSentences(_adc, _tokenizer, _inputAnnotation, _outputAnnotation)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_tokenize_words(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _tokenizer = ToNetObj(inputDict['tokenizer'])
    _inputAnnotation = ToString(inputDict['inputAnnotation'])
    _outputAnnotation = ToString(inputDict['outputAnnotation'])
    execResult = LatinoCF.TokenizeWords(_adc, _tokenizer, _inputAnnotation, _outputAnnotation)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_tokenize_multiple(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _tokenizers = ToNetObj(inputDict['tokenizers'])
    _inputAnnotations = ToString(inputDict['inputAnnotations'])
    _outputAnnotations = ToString(inputDict['outputAnnotations'])
    execResult = LatinoCF.TokenizeMultiple(_adc, _tokenizers, _inputAnnotations, _outputAnnotations)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_tokenize_string_string(inputDict):
    _text = ToNetObj(inputDict['text'])
    _tokenizer = ToNetObj(inputDict['tokenizer'])
    execResult = LatinoCF.TokenizeStringString(_text, _tokenizer)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['string'] = execResultPy
    return outputDict

def latino_tokenize_string_words(inputDict):
    _text = ToNetObj(inputDict['text'])
    _tokenizer = ToNetObj(inputDict['tokenizer'])
    execResult = LatinoCF.TokenizeStringWords(_text, _tokenizer)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['string'] = execResultPy
    return outputDict

def latino_construct_english_maximum_entropy_pos_tagger(inputDict):
    _beamSize = ToInt(inputDict['beamSize'])
    execResult = LatinoCF.ConstructEnglishMaximumEntropyPosTagger(_beamSize)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['posTagger'] = execResultPy
    return outputDict

def latino_pos_tag(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _posTagger = ToNetObj(inputDict['posTagger'])
    _groupAnnotation = ToString(inputDict['groupAnnotation'])
    _elementAnnotation = ToString(inputDict['elementAnnotation'])
    _outputFeature = ToString(inputDict['outputFeature'])
    execResult = LatinoCF.PosTag(_adc, _posTagger, _groupAnnotation, _elementAnnotation, _outputFeature)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_pos_tag_string(inputDict):
    _text = ToNetObj(inputDict['text'])
    _posTagger = ToNetObj(inputDict['posTagger'])
    _outputFeature = ToString(inputDict['outputFeature'])
    execResult = LatinoCF.PosTagString(_text, _posTagger, _outputFeature)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['string'] = execResultPy
    return outputDict

def latino_get_stop_words(inputDict):
    _language = ToEnum(Latino.TextMining.Language, inputDict['language'], Latino.TextMining.Language.English)
    execResult = LatinoCF.GetStopWords(_language)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['strings'] = execResultPy
    return outputDict

def latino_construct_lemma_sharp_lemmatizer(inputDict):
    _language = ToEnum(Latino.TextMining.Language, inputDict['language'], Latino.TextMining.Language.English)
    execResult = LatinoCF.ConstructLemmaSharpLemmatizer(_language)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['tagger'] = execResultPy
    return outputDict

def latino_construct_snowball_stemmer(inputDict):
    _language = ToEnum(Latino.TextMining.Language, inputDict['language'], Latino.TextMining.Language.English)
    execResult = LatinoCF.ConstructSnowballStemmer(_language)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['tagger'] = execResultPy
    return outputDict

def latino_construct_stop_words_tagger(inputDict):
    _stopWords = ToNetObj(inputDict['stopWords'])
    _ignoreCase = ToBool(inputDict['ignoreCase'])
    execResult = LatinoCF.ConstructStopWordsTagger(_stopWords, _ignoreCase)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['tagger'] = execResultPy
    return outputDict

def latino_construct_condition_tagger(inputDict):
    _featureCondition = ToString(inputDict['featureCondition'])
    _outputFeatureValue = ToString(inputDict['outputFeatureValue'])
    _elementsTextToFeatureValue = ToBool(inputDict['elementsTextToFeatureValue'])
    execResult = LatinoCF.ConstructConditionTagger(_featureCondition, _outputFeatureValue, _elementsTextToFeatureValue)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['tagger'] = execResultPy
    return outputDict

def latino_tag_adcstem_lemma(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _tagger = ToNetObj(inputDict['tagger'])
    _elementAnnotation = ToString(inputDict['elementAnnotation'])
    _outputFeature = ToString(inputDict['outputFeature'])
    execResult = LatinoCF.TagADCStemLemma(_adc, _tagger, _elementAnnotation, _outputFeature)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_tag_adcstopwords(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _tagger = ToNetObj(inputDict['tagger'])
    _elementAnnotation = ToString(inputDict['elementAnnotation'])
    _outputFeature = ToString(inputDict['outputFeature'])
    execResult = LatinoCF.TagADCStopwords(_adc, _tagger, _elementAnnotation, _outputFeature)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_tag_adcmultiple(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _taggers = ToNetObj(inputDict['taggers'])
    _elementAnnotations = ToString(inputDict['elementAnnotations'])
    _outputFeatures = ToString(inputDict['outputFeatures'])
    execResult = LatinoCF.TagADCMultiple(_adc, _taggers, _elementAnnotations, _outputFeatures)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['adc'] = execResultPy
    return outputDict

def latino_tag_string_stem_lemma(inputDict):
    _text = ToNetObj(inputDict['text'])
    _tagger = ToNetObj(inputDict['tagger'])
    _outputFeature = ToString(inputDict['outputFeature'])
    execResult = LatinoCF.TagStringStemLemma(_text, _tagger, _outputFeature)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['string'] = execResultPy
    return outputDict

def latino_tag_string_stopwords(inputDict):
    _text = ToNetObj(inputDict['text'])
    _tagger = ToNetObj(inputDict['tagger'])
    _outputFeature = ToString(inputDict['outputFeature'])
    execResult = LatinoCF.TagStringStopwords(_text, _tagger, _outputFeature)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['string'] = execResultPy
    return outputDict

def latino_construct_bow_space_1(inputDict):
    _documents = ToNetObj(inputDict['documents'])
    _tokenizer = ToNetObj(inputDict['tokenizer'])
    _stemmer = ToNetObj(inputDict['stemmer'])
    _stopwords = ToNetObj(inputDict['stopwords'])
    _maxNGramLen = ToInt(inputDict['maxNGramLen'])
    _minWordFreq = ToInt(inputDict['minWordFreq'])
    _wordWeightType = ToEnum(Latino.TextMining.WordWeightType, inputDict['wordWeightType'], Latino.TextMining.WordWeightType.TfIdf)
    _cutLowWeightsPerc = ToFloat(inputDict['cutLowWeightsPerc'])
    _normalizeVectors = ToBool(inputDict['normalizeVectors'])
    execResult = LatinoCF.ConstructBowSpace(_documents, _tokenizer, _stemmer, _stopwords, _maxNGramLen, _minWordFreq, _wordWeightType, _cutLowWeightsPerc, _normalizeVectors)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['bow'] = execResultPy['bow']
    outputDict['ds'] = execResultPy['ds']
    return outputDict

def latino_construct_bow_space_2(inputDict):
    _adc = ToNetObj(inputDict['adc'])
    _tokenId = ToString(inputDict['tokenId'])
    _stemId = ToString(inputDict['stemId'])
    _stopwordId = ToString(inputDict['stopwordId'])
    _labelId = ToString(inputDict['labelId'])
    _maxNGramLen = ToInt(inputDict['maxNGramLen'])
    _minWordFreq = ToInt(inputDict['minWordFreq'])
    _wordWeightType = ToEnum(Latino.TextMining.WordWeightType, inputDict['wordWeightType'], Latino.TextMining.WordWeightType.TfIdf)
    _cutLowWeightsPerc = ToFloat(inputDict['cutLowWeightsPerc'])
    _normalizeVectors = ToBool(inputDict['normalizeVectors'])
    execResult = LatinoCF.ConstructBowSpace(_adc, _tokenId, _stemId, _stopwordId, _labelId, _maxNGramLen, _minWordFreq, _wordWeightType, _cutLowWeightsPerc, _normalizeVectors)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['bow'] = execResultPy['bow']
    outputDict['ds'] = execResultPy['ds']
    return outputDict

def latino_get_vocabulary(inputDict):
    _bow = ToNetObj(inputDict['bow'])
    _startIndex = ToInt(inputDict['startIndex'])
    _maxWords = ToInt(inputDict['maxWords'])
    execResult = LatinoCF.GetVocabulary(_bow, _startIndex, _maxWords)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['vocabulary'] = execResultPy
    return outputDict

def latino_process_new_documents_from_adc(inputDict):
    _bow = ToNetObj(inputDict['bow'])
    _adc = ToNetObj(inputDict['adc'])
    execResult = LatinoCF.ProcessNewDocumentsFromADC(_bow, _adc)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['ds'] = execResultPy
    return outputDict

def latino_process_new_documents_from_string(inputDict):
    _bow = ToNetObj(inputDict['bow'])
    _lst = ToNetObj(inputDict['lst'])
    execResult = LatinoCF.ProcessNewDocumentsFromString(_bow, _lst)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['ds'] = execResultPy
    return outputDict

def latino_add_labels_to_document_vectors(inputDict):
    _ds = ToNetObj(inputDict['ds'])
    _labels = ToNetObj(inputDict['labels'])
    execResult = LatinoCF.AddLabelsToDocumentVectors(_ds, _labels)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['ds'] = execResultPy
    return outputDict

def latino_remove_document_vectors_labels(inputDict):
    _ds = ToNetObj(inputDict['ds'])
    execResult = LatinoCF.RemoveDocumentVectorsLabels(_ds)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['ds'] = execResultPy
    return outputDict

def latino_extract_dataset_labels(inputDict):
    _ds = ToNetObj(inputDict['ds'])
    execResult = LatinoCF.ExtractDatasetLabels(_ds)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['labels'] = execResultPy
    return outputDict

def latino_dataset_split_simple(inputDict):
    _ds = ToNetObj(inputDict['ds'])
    _percentage = ToFloat(inputDict['percentage'])
    _randomSeed = ToInt(inputDict['randomSeed'])
    execResult = LatinoCF.DatasetSplitSimple(_ds, _percentage, _randomSeed)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['ds1'] = execResultPy['ds1']
    outputDict['ds2'] = execResultPy['ds2']
    return outputDict

def latino_dataset_split_predefined(inputDict):
    _ds = ToNetObj(inputDict['ds'])
    _sets = ToNetObj(inputDict['sets'])
    _setId = ToInt(inputDict['setId'])
    execResult = LatinoCF.DatasetSplitPredefined(_ds, _sets, _setId)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['ds1'] = execResultPy['ds1']
    outputDict['ds2'] = execResultPy['ds2']
    return outputDict

def latino_calculate_similarity_matrix(inputDict):
    _ds1 = ToNetObj(inputDict['ds1'])
    _ds2 = ToNetObj(inputDict['ds2'])
    _thresh = ToFloat(inputDict['thresh'])
    _fullMatrix = ToBool(inputDict['fullMatrix'])
    execResult = LatinoCF.CalculateSimilarityMatrix(_ds1, _ds2, _thresh, _fullMatrix)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['smx'] = execResultPy
    return outputDict

def latino_sparse_matrix_to_table(inputDict):
    _smx = ToNetObj(inputDict['smx'])
    execResult = LatinoCF.SparseMatrixToTable(_smx)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['tbl'] = execResultPy
    return outputDict

def latino_construct_kmeans_clusterer(inputDict):
    _k = ToInt(inputDict['k'])
    _centroidType = ToEnum(Latino.Model.CentroidType, inputDict['centroidType'], Latino.Model.CentroidType.NrmL2)
    _similarityModel = ToEnum(LatinoClowdFlows.SimilarityModel, inputDict['similarityModel'], LatinoClowdFlows.SimilarityModel.Cosine)
    _randomSeed = ToInt(inputDict['randomSeed'])
    _eps = ToFloat(inputDict['eps'])
    _trials = ToInt(inputDict['trials'])
    execResult = LatinoCF.ConstructKMeansClusterer(_k, _centroidType, _similarityModel, _randomSeed, _eps, _trials)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['clusterer'] = execResultPy
    return outputDict

def latino_construct_kmeans_fast_clusterer(inputDict):
    _k = ToInt(inputDict['k'])
    _randomSeed = ToInt(inputDict['randomSeed'])
    _eps = ToFloat(inputDict['eps'])
    _trials = ToInt(inputDict['trials'])
    execResult = LatinoCF.ConstructKMeansFastClusterer(_k, _randomSeed, _eps, _trials)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['clusterer'] = execResultPy
    return outputDict

def latino_construct_hierarchical_bisecting_clusterer(inputDict):
    _minQuality = ToFloat(inputDict['minQuality'])
    execResult = LatinoCF.ConstructHierarchicalBisectingClusterer(_minQuality)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['clusterer'] = execResultPy
    return outputDict

def latino_cluster_document_vectors(inputDict):
    _clusterer = ToNetObj(inputDict['clusterer'])
    _dataset = ToNetObj(inputDict['dataset'])
    execResult = LatinoCF.ClusterDocumentVectors(_clusterer, _dataset)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['clustRes'] = execResultPy
    return outputDict

def latino_clustering_results_info(inputDict):
    _clustRes = ToNetObj(inputDict['clustRes'])
    execResult = LatinoCF.ClusteringResultsInfo(_clustRes)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['docLabels'] = execResultPy['docLabels']
    outputDict['clustTree'] = execResultPy['clustTree']
    return outputDict

def latino_construct_centroid_classifier(inputDict):
    _similarityModel = ToEnum(LatinoClowdFlows.SimilarityModel, inputDict['similarityModel'], LatinoClowdFlows.SimilarityModel.Cosine)
    _normalizeCentorids = ToBool(inputDict['normalizeCentorids'])
    execResult = LatinoCF.ConstructCentroidClassifier(_similarityModel, _normalizeCentorids)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['classifier'] = execResultPy
    return outputDict

def latino_construct_naive_bayes_classifier(inputDict):
    _normalize = ToBool(inputDict['normalize'])
    _logSumExpTrick = ToBool(inputDict['logSumExpTrick'])
    execResult = LatinoCF.ConstructNaiveBayesClassifier(_normalize, _logSumExpTrick)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['classifier'] = execResultPy
    return outputDict

def latino_construct_svm_binary_classifier(inputDict):
    _c = ToFloat(inputDict['c'])
    _biasedHyperplane = ToBool(inputDict['biasedHyperplane'])
    _kernelType = ToEnum(Latino.Model.SvmLightKernelType, inputDict['kernelType'], Latino.Model.SvmLightKernelType.Linear)
    _kernelParamGamma = ToFloat(inputDict['kernelParamGamma'])
    _kernelParamD = ToFloat(inputDict['kernelParamD'])
    _kernelParamS = ToFloat(inputDict['kernelParamS'])
    _kernelParamC = ToFloat(inputDict['kernelParamC'])
    _eps = ToFloat(inputDict['eps'])
    _maxIter = ToInt(inputDict['maxIter'])
    _customParams = ToString(inputDict['customParams'])
    execResult = LatinoCF.ConstructSvmBinaryClassifier(_c, _biasedHyperplane, _kernelType, _kernelParamGamma, _kernelParamD, _kernelParamS, _kernelParamC, _eps, _maxIter, _customParams)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['classifier'] = execResultPy
    return outputDict

def latino_construct_svm_multiclass_fast(inputDict):
    _c = ToFloat(inputDict['c'])
    _eps = ToFloat(inputDict['eps'])
    execResult = LatinoCF.ConstructSvmMulticlassFast(_c, _eps)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['classifier'] = execResultPy
    return outputDict

def latino_construct_majority_classifier(inputDict):
    execResult = LatinoCF.ConstructMajorityClassifier()
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['classifier'] = execResultPy
    return outputDict

def latino_construct_maximum_entropy_classifier(inputDict):
    _moveData = ToBool(inputDict['moveData'])
    _numIter = ToInt(inputDict['numIter'])
    _cutOff = ToInt(inputDict['cutOff'])
    _numThreads = ToInt(inputDict['numThreads'])
    _normalize = ToBool(inputDict['normalize'])
    execResult = LatinoCF.ConstructMaximumEntropyClassifier(_moveData, _numIter, _cutOff, _numThreads, _normalize)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['classifier'] = execResultPy
    return outputDict

def latino_construct_maximum_entropy_classifier_fast(inputDict):
    _moveData = ToBool(inputDict['moveData'])
    _numIter = ToInt(inputDict['numIter'])
    _cutOff = ToInt(inputDict['cutOff'])
    _numThreads = ToInt(inputDict['numThreads'])
    _normalize = ToBool(inputDict['normalize'])
    execResult = LatinoCF.ConstructMaximumEntropyClassifierFast(_moveData, _numIter, _cutOff, _numThreads, _normalize)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['classifier'] = execResultPy
    return outputDict

def latino_construct_knn_classifier(inputDict):
    _similarityModel = ToEnum(LatinoClowdFlows.SimilarityModel, inputDict['similarityModel'], LatinoClowdFlows.SimilarityModel.Cosine)
    _k = ToInt(inputDict['k'])
    _softVoting = ToBool(inputDict['softVoting'])
    execResult = LatinoCF.ConstructKnnClassifier(_similarityModel, _k, _softVoting)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['classifier'] = execResultPy
    return outputDict

def latino_construct_knn_classifier_fast(inputDict):
    _k = ToInt(inputDict['k'])
    _softVoting = ToBool(inputDict['softVoting'])
    execResult = LatinoCF.ConstructKnnClassifierFast(_k, _softVoting)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['classifier'] = execResultPy
    return outputDict

def latino_train_classifier(inputDict):
    _csf = ToNetObj(inputDict['csf'])
    _ds = ToNetObj(inputDict['ds'])
    execResult = LatinoCF.TrainClassifier(_csf, _ds)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['csf'] = execResultPy
    return outputDict

def latino_predict_classification(inputDict):
    _csf = ToNetObj(inputDict['csf'])
    _ds = ToNetObj(inputDict['ds'])
    execResult = LatinoCF.PredictClassification(_csf, _ds)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['predictions'] = execResultPy['predictions']
    outputDict['ds'] = execResultPy['ds']
    return outputDict

def latino_prediction_info(inputDict):
    _predictions = ToNetObj(inputDict['predictions'])
    execResult = LatinoCF.PredictionInfo(_predictions)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['labels'] = execResultPy['labels']
    outputDict['predictInfos'] = execResultPy['predictInfos']
    return outputDict

def latino_cross_validation(inputDict):
    _csf = ToNetObj(inputDict['csf'])
    _ds = ToNetObj(inputDict['ds'])
    _numOfSets = ToInt(inputDict['numOfSets'])
    _random = ToBool(inputDict['random'])
    _useSeed = ToBool(inputDict['useSeed'])
    _randomSeed = ToInt(inputDict['randomSeed'])
    execResult = LatinoCF.CrossValidation(_csf, _ds, _numOfSets, _random, _useSeed, _randomSeed)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['obj'] = execResultPy
    return outputDict

def latino_cross_validation_predef_splits(inputDict):
    _csf = ToNetObj(inputDict['csf'])
    _ds = ToNetObj(inputDict['ds'])
    _sets = ToNetObj(inputDict['sets'])
    execResult = LatinoCF.CrossValidationPredefSplits(_csf, _ds, _sets)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['obj'] = execResultPy
    return outputDict

def latino_cross_validation_predef_multi_splits(inputDict):
    _csf = ToNetObj(inputDict['csf'])
    _ds = ToNetObj(inputDict['ds'])
    _multiSets = ToNetObj(inputDict['multiSets'])
    execResult = LatinoCF.CrossValidationPredefMultiSplits(_csf, _ds, _multiSets)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['obj'] = execResultPy
    return outputDict

def latino_run_csharp_snippet(inputDict):
    _snippetParams = ToNetObj(inputDict['snippetParams'])
    _snippetCode = ToString(inputDict['snippetCode'])
    _aditionalReferences = ToString(inputDict['aditionalReferences'])
    _usings = ToString(inputDict['usings'])
    execResult = LatinoCF.RunCSharpSnippet(_snippetParams, _snippetCode, _aditionalReferences, _usings)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['out'] = execResultPy['out']
    outputDict['consoleOut'] = execResultPy['consoleOut']
    outputDict['error'] = execResultPy['error']
    outputDict['code'] = execResultPy['code']
    return outputDict

