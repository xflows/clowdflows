import latino
import logging

# Loading and saving adc files
def load_adc(input_dict):
    logging.info('__load_adc__')
    output_dict = {}
    output_dict['adc'] = latino.loadADC(input_dict['file'], input_dict['leadingLabels'])
    return output_dict
def load_adc_from_string(input_dict):
    logging.info('__load_adc_from_string__')
    output_dict = {}
    output_dict['adc'] = latino.loadADCFromString(input_dict['string'], input_dict['leadingLabels'])
    return output_dict
def get_strings(input_dict):
    logging.info('__get_strings__')
    output_dict = {}
    output_dict['strings'] = latino.getStrings(input_dict['adc'], input_dict['elementAnnotation'], input_dict['elementFeatureConditions'], input_dict['delimiter'], input_dict['includeDocId'])
    return output_dict
def extract_documents_features(input_dict):
    logging.info('__extract_documents_features__')
    output_dict = {}
    output_dict['strings'] = latino.ExtractDocumentsFeatures(input_dict['adc'], input_dict['featureName'])
    return output_dict
def add_documents_features(input_dict):
    logging.info('__add_documents_features__')
    output_dict = {}
    output_dict['adc'] = latino.AddDocumentsFeatures(input_dict['adc'], input_dict['featureValues'], input_dict['featureName'], input_dict['featureValuePrefix'])
    return output_dict
def split_documents_by_feature_value(input_dict):
    logging.info('__split_documents_by_feature_value__')
    output_dict = latino.SplitDocumentsByFeatureValue(input_dict['adc'], input_dict['featureCondition'])
    return output_dict
def mark_documents_with_sets(input_dict):
    logging.info('__mark_documents_with_sets__')
    output_dict = {}
    output_dict['adc'] = latino.MarkDocumentsWithSetFeature(input_dict['adc'], input_dict['featureName'], input_dict['featureValuePrefix'], \
        input_dict['numOfSets'], input_dict['random'], input_dict['useSeed'], input_dict['randomSeed'])
    return output_dict
def corpus_stataistics(input_dict):
    logging.info('__corpus_stataistics__')
    output_dict = latino.CorpusStatistics(input_dict['adc'])
    return output_dict

# ADC conversion
def adc2xml(input_dict):
    logging.info('__adc2xml__')
    output_dict = {}
    output_dict['string'] = latino.ADCtoXMLString(input_dict['adc'])
    return output_dict
def xml2adc(input_dict):
    logging.info('__xml2adc__')
    output_dict = {}
    output_dict['adc'] = latino.XMLStringtoADC(input_dict['string'])
    return output_dict

# Text tokenization
def create_english_me_sentence_detector(input_dict):
    logging.info('__create_english_me_sentence_detector__')
    output_dict = {}
    output_dict['tokenizer'] = latino.CreateEnglishMaximumEntropySentenceDetector(True)
    return output_dict
def create_english_me_tokenizer(input_dict):
    logging.info('__create_english_me_tokenizer__')
    output_dict = {}
    output_dict['tokenizer'] = latino.CreateEnglishMaximumEntropyTokenizer(True, input_dict['alphaNumericOptimization'])
    return output_dict
def create_unicode_tokenizer(input_dict):
    logging.info('__create_unicode_tokenizer__')
    output_dict = {}
    output_dict['tokenizer'] = latino.CreateUnicodeTokenizer(input_dict['filter'], input_dict['minTokenLen'])
    return output_dict
def create_simple_tokenizer(input_dict):
    logging.info('__create_simple_tokenizer__')
    output_dict = {}
    output_dict['tokenizer'] = latino.CreateSimpleTokenizer(input_dict['type'], input_dict['minTokenLen'])
    return output_dict
def create_regex_tokenizer(input_dict):
    logging.info('__create_regex_tokenizer__')
    output_dict = {}
    output_dict['tokenizer'] = latino.CreateRegexTokenizer(input_dict['tokenRegex'], input_dict['ignoreUnknownTokens'],
        'false', input_dict['regexOptionsIgnoreCase'], input_dict['regexOptionsMultiline'],
        input_dict['regexOptionsExplicitCapture'], "true", input_dict['regexOptionsSingleline'],
        input_dict['regexOptionsIgnorePatternWhitespace'], input_dict['regexOptionsRightToLeft'], input_dict['regexOptionsECMAScript'],
        input_dict['regexOptionsCultureInvariant'])
    return output_dict
def tokenize(input_dict):
    logging.info('__tokenize__')
    output_dict = {}
    output_dict['adc'] = latino.Tokenize(input_dict['adc'], input_dict['tokenizer'], input_dict['inputAnnotation'], input_dict['outputAnnotation'])
    return output_dict
def tokenize_string(input_dict):
    logging.info('__tokenize_string__')
    output_dict = {}
    output_dict['string'] = latino.TokenizeString(input_dict['string'], input_dict['tokenizer'])
    return output_dict
def tokenize_multiple(input_dict):
    logging.info('__tokenize_multiple__')
    output_dict = {}
    output_dict['adc'] = latino.TokenizeMultiple(input_dict['adc'], input_dict['tokenizer'], input_dict['inputAnnotation'], input_dict['outputAnnotation'])
    return output_dict

# POS Tagging
def create_english_me_postagger(input_dict):
    logging.info('__create_english_me_postagger__')
    output_dict = {}
    output_dict['posTagger'] = latino.CreateEnglishMaximumEntropyPosTagger(input_dict['beamSize'], True)
    return output_dict
def pos_tag(input_dict):
    logging.info('__pos_tag__')
    output_dict = {}
    output_dict['adc'] = latino.PosTag(input_dict['adc'], input_dict['posTagger'], input_dict['groupAnnotation'], input_dict['elementAnnotation'], input_dict['outputFeature'])
    return output_dict
def pos_tag_string(input_dict):
    logging.info('__pos_tag_string__')
    output_dict = {}
    output_dict['string'] = latino.PosTagString(input_dict['string'], input_dict['posTagger'], input_dict['outputFeature'])
    return output_dict

# Tagging
def create_lemmasharp_lemmatizer(input_dict):
    logging.info('__create_lemmasharp_lemmatizer__')
    output_dict = {}
    output_dict['tagger'] = latino.ConstructLemmaSharpLemmatizer(input_dict['language'])
    return output_dict
def create_snowball_stemmer(input_dict):
    logging.info('__create_snowball_stemmer__')
    output_dict = {}
    output_dict['tagger'] = latino.ConstructSnowballStemmer(input_dict['language'])
    return output_dict
def create_stopwords_tagger(input_dict):
    logging.info('__create_stopwords_tagger__')
    output_dict = {}
    output_dict['tagger'] = latino.ConstructStopWordsTagger(input_dict['stopWords'], input_dict['ignoreCase'])
    return output_dict
def create_condition_tagger(input_dict):
    logging.info('__create_condition_tagger__')
    output_dict = {}
    output_dict['tagger'] = latino.ConstructConditionTagger(input_dict['featureCondition'], input_dict['outputFeatureValue'], input_dict['elementsTextToFeatureValue'])
    return output_dict
def tag_string(input_dict):
    logging.info('__tag_string__')
    output_dict = {}
    output_dict['string'] = latino.TagString(input_dict['string'], input_dict['tagger'], input_dict['outputFeature'])
    return output_dict
def tag_adc(input_dict):
    logging.info('__tag_adc__')
    output_dict = {}
    output_dict['adc'] = latino.TagADC(input_dict['adc'], input_dict['tagger'], input_dict['elementAnnotation'], input_dict['outputFeature'])
    return output_dict
def tag_adc_multiple(input_dict):
    logging.info('__tag_adc_multiple__')
    output_dict = {}
    output_dict['adc'] = latino.TagADCMultiple(input_dict['adc'], input_dict['tagger'], input_dict['elementAnnotation'], input_dict['outputFeature'])
    return output_dict

# Tagging helper functions
def get_stopwords(input_dict):
    logging.info('__get_stopwords__')
    output_dict = {}
    output_dict['strings'] = latino.GetStopWords(input_dict['language'])
    return output_dict

# Bow creation and manipulation
def create_bow_space_from_texts(input_dict):
    logging.info('__create_bow_space_from_texts__')
    output_dict = latino.ConstructBowSpaceFromTexts(input_dict['documents'], input_dict['tokenizer'], input_dict['stemmer'], input_dict['stopwords'], \
        input_dict['maxNGramLen'], input_dict['minWordFreq'], input_dict['wordWeightType'], input_dict['cutLowWeightsPerc'], input_dict['normalizeVectors'])
    return output_dict
def create_bow_space_from_adc(input_dict):
    logging.info('__create_bow_space_from_adc__')
    output_dict = latino.ConstructBowSpaceFromADC(input_dict['adc'], input_dict['tokenId'], input_dict['stemId'], input_dict['stopwordId'], input_dict['labelId'],\
        input_dict['maxNGramLen'], input_dict['minWordFreq'], input_dict['wordWeightType'], input_dict['cutLowWeightsPerc'], input_dict['normalizeVectors'])
    return output_dict

def bow_get_vocabulary(input_dict):
    logging.info('__bow_get_vocabulary__')
    output_dict = {}
    output_dict['vocabulary'] = latino.GetVocabulary(input_dict['bow'], input_dict['startIndex'], input_dict['maxWords'])
    return output_dict
def bow_get_document_vectors(input_dict):
    logging.info('__bow_get_document_vectors__')
    output_dict = {}
    output_dict['ds'] = latino.GetDocumentVectors(input_dict['bow'])
    return output_dict
def bow_process_new_documents(input_dict):
    logging.info('__bow_process_new_documents__')
    output_dict = {}
    output_dict['ds'] = latino.ProcessNewDocuments(input_dict['bow'], input_dict['obj'])
    return output_dict

# Document vectors manipulation
def get_labeled_dataset(input_dict):
    logging.info('__get_labeled_dataset__')
    output_dict = {}
    output_dict['ds'] = latino.AddLabelsToDocumentVectors(input_dict['ds'], input_dict['labels'])
    return output_dict
def get_unlabeled_dataset(input_dict):
    logging.info('__get_unlabeled_dataset__')
    output_dict = {}
    output_dict['ds'] = latino.RemoveDocumentVectorsLabels(input_dict['ds'])
    return output_dict
def extract_dataset_labels(input_dict):
    logging.info('__extract_dataset_labels__')
    output_dict = {}
    output_dict['labels'] = latino.ExtractDatasetLabels(input_dict['ds'])
    return output_dict
def dataset_split_simple(input_dict):
    logging.info('__dataset_split_simple__')
    return latino.DatasetSplitSimple(input_dict)

def calculate_similarity_matrix(input_dict):
    logging.info('__calculate_similarity_matrix__')
    output_dict = {}
    output_dict["smx"] = latino.CalculateSimilarityMatrix(input_dict['dv1'], input_dict['dv2'], input_dict['thresh'], input_dict['fullMatrix'])
    return output_dict
def matrix_to_table(input_dict):
    logging.info('__matrix_to_table__')
    output_dict = {}
    output_dict["tbl"] = latino.SparseMatrixToTable(input_dict['smx'])
    return output_dict

# Clustering
def create_kmeans_clusterer(input_dict):
    logging.info('__create_kmeans_clusterer__')
    output_dict = {}
    output_dict['clusterer'] = latino.ConstructKMeansClusterer(input_dict['k'], input_dict['centroidType'], input_dict['similarityModel'], input_dict['randomSeed'], input_dict['eps'], input_dict['trials'])
    return output_dict
def create_kmeans_fast_clusterer(input_dict):
    logging.info('__create_kmeans_fast_clusterer__')
    output_dict = {}
    output_dict['clusterer'] = latino.ConstructKMeansFastClusterer(input_dict['k'], input_dict['randomSeed'], input_dict['eps'], input_dict['trials'])
    return output_dict
def create_hier_bisect_clusterer(input_dict):
    logging.info('__create_hier_bisect_clusterer__')
    output_dict = {}
    output_dict['clusterer'] = latino.ConstructHierarchicalBisectingClusterer(input_dict['minQuality'])
    return output_dict

def cluster_document_vectors(input_dict):
    logging.info('__cluster_document_vectors__')
    output_dict = {}
    output_dict['clustRes'] = latino.ClusterDocumentVectors(input_dict['clusterer'], input_dict['ds'])
    return output_dict
def clustering_results_info(input_dict):
    logging.info('__clustering_results_info__')
    dict = latino.ClusteringResultsInfo(input_dict['clustRes'])
    output_dict = {'docLabels':dict['docLabels'],'clustTree':dict['clustTree']}
    return output_dict

# Classification
def construct_centroid_classifier(input_dict):
    logging.info('__construct_centroid_classifier__')
    return latino.ConstructCentroidClassifier(input_dict)
def construct_naivebayes_classifier(input_dict):
    logging.info('__construct_naivebayes_classifier__')
    return latino.ConstructNaiveBayesClassifier(input_dict)
def construct_svm_binary_classifier(input_dict):
    logging.info('__construct_svm_binary_classifier__')
    return latino.ConstructSvmBinaryClassifier(input_dict)
def construct_svm_multiclass_fast(input_dict):
    logging.info('__construct_svm_multiclass_fast__')
    return latino.ConstructSvmMulticlassFast(input_dict)
def construct_majority_classifier(input_dict):
    logging.info('__construct_majority_classifier__')
    return latino.ConstructMajorityClassifier(input_dict)
def construct_maximum_entropy_classifier(input_dict):
    logging.info('__construct_maximum_entropy_classifier__')
    return latino.ConstructMaximumEntropyClassifier(input_dict)
def construct_maximum_entropy_classifier_fast(input_dict):
    logging.info('__construct_maximum_entropy_classifier_fast__')
    return latino.ConstructMaximumEntropyClassifierFast(input_dict)
def construct_knn_classifier(input_dict):
    logging.info('__construct_knn_classifier__')
    return latino.ConstructKnnClassifier(input_dict)
def construct_knn_classifier_fast(input_dict):
    logging.info('__construct_knn_classifier_fast__')
    return latino.ConstructKnnClassifierFast(input_dict)

def train_classifier(input_dict):
    logging.info('__train_classifier__')
    output_dict = {}
    output_dict['classifier'] = latino.TrainClassifier(input_dict['classifier'], input_dict['ds'])
    return output_dict

def predict_classification(input_dict):
    logging.info('__predict_classification__')
    output_dict = latino.PredictClassification(input_dict['classifier'], input_dict['ds'])
    return output_dict

def prediction_to_label(input_dict):
    logging.info('__prediction_to_label__')
    output_dict = {}
    output_dict['labels'] = latino.PredictionToLabel(input_dict['predictions'])
    return output_dict
def prediction_info(input_dict):
    logging.info('__prediction_info__')
    output_dict = latino.PredictionInfo(input_dict['predictions'])
    return output_dict

def cross_validation(input_dict):
    logging.info('__cross_validation__')
    output_dict = {}
    output_dict['obj'] = latino.CrossValidation(input_dict['csf'], input_dict['ds'], input_dict['numOfSets'], input_dict['random'], input_dict['useSeed'], input_dict['randomSeed'])
    return output_dict
def cross_validation_predef_splits(input_dict):
    logging.info('__cross_validation_predef_splits__')
    output_dict = {}
    output_dict['obj'] = latino.CrossValidationPredefSplits(input_dict['csf'], input_dict['ds'], input_dict['sets'])
    return output_dict

#------------------------------------------------------------------------------
# VISUALISATIONS
#------------------------------------------------------------------------------
def show_adc(input_dict):
    logging.info('__show_adc__FUNCT__')
    if input_dict["adc"] == None:
        raise Exception("Input ADC is required for displaying Anotated Document Corpus!")
    return {}
def advanced_object_viewer(input_dict):
    logging.info('__advanced_object_viewer__FUNCT__')
    return {}
def show_table(input_dict):
    logging.info('__show_table__FUNCT__')
    return {}

#------------------------------------------------------------------------------
# SUPPLEMENTARY FUNCTIONS
#------------------------------------------------------------------------------
def object_splitter(input_dict):
    logging.info('__get_stopwords__')
    output_dict = {}
    obj = input_dict['object']
    output_dict['object'] = eval("obj"+input_dict['attribute'])
    return output_dict
def python_snippet(input_dict):
    logging.info('__python_snippet__')
    output_dict = {}
    input = input_dict['in']
    for (i, val) in enumerate(input):
        vars()["in"+str(i+1)] = val
    out1 = None
    exec(input_dict['pycode'])
    output_dict['out'] = out1
    return output_dict
def csharp_snippet(input_dict):
    logging.info('__csharp_snippet__')
    output_dict = latino.RunCSharpSnippet(input_dict['in'], input_dict['snippetCode'], input_dict['aditionalReferences'], input_dict['usings'])
    return output_dict
def flatten_to_string_array(input_dict):
    logging.info('__flatten_to_string_array__')
    output_dict = {}
    obj = input_dict['object']
    output_dict['object'] = latino.FlattenObjectToStringArray(obj)
    return output_dict
def create_range(input_dict):
    rng = range(latino.ToInt(input_dict['start']), latino.ToInt(input_dict['stop']), latino.ToInt(input_dict['step']))
    return {'range':rng }
def compare_lists(input_dict):
    l1 = input_dict['list1']
    l2 = input_dict['list2']
    l = min(len(l1),len(l2))
    cntEq = 0;
    for i in range(0,l):
        if l1[i]==l2[i]:
            cntEq += 1
    return {
        'accuracy':(0.0+cntEq)/l,
        'statistics':{
            'elements':l,
            'equal':cntEq,
            'different':l-cntEq,
            'accuracy':(0.0+cntEq)/l,
            'error':(0.0+l-cntEq)/l,
        }
    }



