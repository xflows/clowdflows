# -----------------------------------------------------------------------------------------------------
# WARNING: THIS IS AUTOMATICALLY GENERATED FILE, DO NOT EDIT IT MANUALLY AS YOU MAY LOOSE YOUR CHANGES!
# -----------------------------------------------------------------------------------------------------

from import_dotnet import *
from serialization_utils import *

def lemmagen_load_example_list_from_string(inputDict):
    _tabDelim = ToString(inputDict['tabDelim'])
    _format = ToString(inputDict['format'])
    execResult = LemmaSharpIntf.LoadExampleListFromString(_tabDelim, _format)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['exampleList'] = execResultPy
    return outputDict

def lemmagen_load_example_list_from_table(inputDict):
    _table = ToNetObj(inputDict['table'])
    _format = ToString(inputDict['format'])
    execResult = LemmaSharpIntf.LoadExampleListFromTable(_table, _format)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['exampleList'] = execResultPy
    return outputDict

def lemmagen_example_list_to_table(inputDict):
    _exampleList = ToNetObj(inputDict['exampleList'])
    execResult = LemmaSharpIntf.ExampleListToTable(_exampleList)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['table'] = execResultPy
    return outputDict

def lemmagen_save_lemmatizer(inputDict):
    _lmtz = ToNetObj(inputDict['lmtz'])
    execResult = LemmaSharpIntf.SaveLemmatizer(_lmtz)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['objString'] = execResultPy
    return outputDict

def lemmagen_load_net_lemmatizer(inputDict):
    _lmtzStr = ToString(inputDict['lmtzStr'])
    execResult = LemmaSharpIntf.LoadNetLemmatizer(_lmtzStr)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['obj'] = execResultPy
    return outputDict

def lemmagen_group_examples(inputDict):
    _exampleList = ToNetObj(inputDict['exampleList'])
    _ignoreFrequencies = ToBool(inputDict['ignoreFrequencies'])
    _msdConsider = ToEnum(LemmaSharp.LemmatizerSettings.MsdConsideration, inputDict['msdConsider'], LemmaSharp.LemmatizerSettings.MsdConsideration.Distinct)
    execResult = LemmaSharpIntf.GroupExamples(_exampleList, _ignoreFrequencies, _msdConsider)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['exampleList'] = execResultPy
    return outputDict

def lemmagen_construct_lemmatizer_settings(inputDict):
    _useFromInRules = ToBool(inputDict['useFromInRules'])
    _msdConsider = ToEnum(LemmaSharp.LemmatizerSettings.MsdConsideration, inputDict['msdConsider'], LemmaSharp.LemmatizerSettings.MsdConsideration.Distinct)
    _maxRulesPerNode = ToInt(inputDict['maxRulesPerNode'])
    _buildFrontLemmatizer = ToBool(inputDict['buildFrontLemmatizer'])
    _storeAllFullKnownWords = ToBool(inputDict['storeAllFullKnownWords'])
    execResult = LemmaSharpIntf.ConstructLemmatizerSettings(_useFromInRules, _msdConsider, _maxRulesPerNode, _buildFrontLemmatizer, _storeAllFullKnownWords)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['lemmatizerSettings'] = execResultPy
    return outputDict

def lemmagen_extract_lemmatizer_settings(inputDict):
    _lmtz = ToNetObj(inputDict['lmtz'])
    execResult = LemmaSharpIntf.ExtractLemmatizerSettings(_lmtz)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['lemmatizerSettings'] = execResultPy
    return outputDict

def lemmagen_construct_prebuild_lemmatizer(inputDict):
    _language = ToEnum(LemmaSharp.LanguagePrebuilt, inputDict['language'], LemmaSharp.LanguagePrebuilt.English)
    execResult = LemmaSharpIntf.ConstructPrebuildLemmatizer(_language)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['lemamtizer'] = execResultPy
    return outputDict

def lemmagen_construct_lemmatizer(inputDict):
    _lemmatizerSettings = ToNetObj(inputDict['lemmatizerSettings'])
    _exampleList = ToNetObj(inputDict['exampleList'])
    execResult = LemmaSharpIntf.ConstructLemmatizer(_lemmatizerSettings, _exampleList)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['lemmatizer'] = execResultPy
    return outputDict

def lemmagen_display_lemmatization_rules(inputDict):
    _lmtz = ToNetObj(inputDict['lmtz'])
    execResult = LemmaSharpIntf.DisplayLemmatizationRules(_lmtz)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['lmtzTree'] = execResultPy
    return outputDict

def lemmagen_lemmatize_words(inputDict):
    _lemmatizer = ToNetObj(inputDict['lemmatizer'])
    _words = ToNetObj(inputDict['words'])
    _leaveWord = ToBool(inputDict['leaveWord'])
    _ignoreCase = ToBool(inputDict['ignoreCase'])
    _msd = ToString(inputDict['msd'])
    execResult = LemmaSharpIntf.LemmatizeWords(_lemmatizer, _words, _leaveWord, _ignoreCase, _msd)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['lemmas'] = execResultPy
    return outputDict

def lemmagen_lemmatize_explain_words(inputDict):
    _lemmatizer = ToNetObj(inputDict['lemmatizer'])
    _words = ToNetObj(inputDict['words'])
    _ignoreCase = ToBool(inputDict['ignoreCase'])
    _msd = ToString(inputDict['msd'])
    execResult = LemmaSharpIntf.LemmatizeExplainWords(_lemmatizer, _words, _ignoreCase, _msd)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['explanations'] = execResultPy
    return outputDict

def lemmagen_delimited_file2_table(inputDict):
    _file = ToString(inputDict['file'])
    _delimiter = ToString(inputDict['delimiter'])
    _firstLineIsHeader = ToBool(inputDict['firstLineIsHeader'])
    _headerLine = ToString(inputDict['headerLine'])
    _skipEmptyLines = ToBool(inputDict['skipEmptyLines'])
    execResult = LemmaSharpIntf.DelimitedFile2Table(_file, _delimiter, _firstLineIsHeader, _headerLine, _skipEmptyLines)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['table'] = execResultPy
    return outputDict

def lemmagen_delimited_string2_table(inputDict):
    _examplesText = ToString(inputDict['examplesText'])
    _delimiter = ToString(inputDict['delimiter'])
    _firstLineIsHeader = ToBool(inputDict['firstLineIsHeader'])
    _headerLine = ToString(inputDict['headerLine'])
    _skipEmptyLines = ToBool(inputDict['skipEmptyLines'])
    execResult = LemmaSharpIntf.DelimitedString2Table(_examplesText, _delimiter, _firstLineIsHeader, _headerLine, _skipEmptyLines)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['table'] = execResultPy
    return outputDict

def lemmagen_filter_table_rows(inputDict):
    _table = ToNetObj(inputDict['table'])
    _indexList = ToNetObj(inputDict['indexList'])
    _discardFilteredOut = ToBool(inputDict['discardFilteredOut'])
    execResult = LemmaSharpIntf.FilterTableRows(_table, _indexList, _discardFilteredOut)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['tableSelected'] = execResultPy['tableSelected']
    outputDict['tableFiltered'] = execResultPy['tableFiltered']
    return outputDict

def lemmagen_extract_column_as_list(inputDict):
    _table = ToNetObj(inputDict['table'])
    _columnIndex = ToInt(inputDict['columnIndex'])
    execResult = LemmaSharpIntf.ExtractColumnAsList(_table, _columnIndex)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['columnValues'] = execResultPy['columnValues']
    outputDict['columnName'] = execResultPy['columnName']
    return outputDict

def lemmagen_insert_list_as_column(inputDict):
    _table = ToNetObj(inputDict['table'])
    _columnIndex = ToInt(inputDict['columnIndex'])
    _columnValues = ToNetObj(inputDict['columnValues'])
    _columnName = ToString(inputDict['columnName'])
    execResult = LemmaSharpIntf.InsertListAsColumn(_table, _columnIndex, _columnValues, _columnName)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['table'] = execResultPy
    return outputDict

def lemmagen_table2_string_delimited(inputDict):
    _table = ToNetObj(inputDict['table'])
    _delimiter = ToString(inputDict['delimiter'])
    _outputHeader = ToBool(inputDict['outputHeader'])
    execResult = LemmaSharpIntf.Table2StringDelimited(_table, _delimiter, _outputHeader)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['string'] = execResultPy
    return outputDict

def lemmagen_get_set_indexes(inputDict):
    _selectedSetId = ToInt(inputDict['selectedSetId'])
    _setList = ToNetObj(inputDict['setList'])
    _opposite = ToBool(inputDict['opposite'])
    execResult = LemmaSharpIntf.GetSetIndexes(_selectedSetId, _setList, _opposite)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['setIndexes'] = execResultPy
    return outputDict

def lemmagen_filter_list_elements(inputDict):
    _list = ToNetObj(inputDict['list'])
    _indexList = ToNetObj(inputDict['indexList'])
    _discardFilteredOut = ToBool(inputDict['discardFilteredOut'])
    execResult = LemmaSharpIntf.FilterListElements(_list, _indexList, _discardFilteredOut)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['listSelected'] = execResultPy['listSelected']
    outputDict['listFiltered'] = execResultPy['listFiltered']
    return outputDict

def lemmagen_save_net_object(inputDict):
    _serializableObject = ToNetObj(inputDict['serializableObject'])
    _compress = ToBool(inputDict['compress'])
    execResult = LemmaSharpIntf.SaveNetObject(_serializableObject, _compress)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['objString'] = execResultPy
    return outputDict

def lemmagen_load_net_object(inputDict):
    _serializableObjectStr = ToString(inputDict['serializableObjectStr'])
    _compress = ToBool(inputDict['compress'])
    execResult = LemmaSharpIntf.LoadNetObject(_serializableObjectStr, _compress)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['obj'] = execResultPy
    return outputDict

