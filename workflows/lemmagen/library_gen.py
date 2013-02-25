# -----------------------------------------------------------------------------------------------------
# WARNING: THIS IS AUTOMATICALLY GENERATED FILE, DO NOT EDIT IT MANUALLY AS YOU MAY LOOSE YOUR CHANGES!
# -----------------------------------------------------------------------------------------------------

from import_dotnet import *
from serialization_utils import *

def lemmagen_construct_prebuild_lemmatizer(inputDict):
    _language = ToEnum(LemmaSharp.LanguagePrebuilt, inputDict['language'], LemmaSharp.LanguagePrebuilt.English)
    execResult = LemmaSharpIntf.ConstructPrebuildLemmatizer(_language)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['lemamtizer'] = execResultPy
    return outputDict

def lemmagen_construct_lemmatizer(inputDict):
    _useFromInRules = ToBool(inputDict['useFromInRules'])
    _msdConsider = ToEnum(LemmaSharp.LemmatizerSettings+MsdConsideration, inputDict['msdConsider'], LemmaSharp.LemmatizerSettings+MsdConsideration.Distinct)
    _maxRulesPerNode = ToInt(inputDict['maxRulesPerNode'])
    _buildFrontLemmatizer = ToBool(inputDict['buildFrontLemmatizer'])
    _storeAllFullKnownWords = ToBool(inputDict['storeAllFullKnownWords'])
    _useMsdSplitTreeOptimization = ToBool(inputDict['useMsdSplitTreeOptimization'])
    _msdSpec = ToString(inputDict['msdSpec'])
    _exampleList = ToNetObj(inputDict['exampleList'])
    execResult = LemmaSharpIntf.ConstructLemmatizer(_useFromInRules, _msdConsider, _maxRulesPerNode, _buildFrontLemmatizer, _storeAllFullKnownWords, _useMsdSplitTreeOptimization, _msdSpec, _exampleList)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['lemmatizer'] = execResultPy
    return outputDict

def lemmagen_lemmatize_words(inputDict):
    _lemmatizer = ToNetObj(inputDict['lemmatizer'])
    _words = ToNetObj(inputDict['words'])
    _leaveWord = ToBool(inputDict['leaveWord'])
    execResult = LemmaSharpIntf.LemmatizeWords(_lemmatizer, _words, _leaveWord)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['lemmas'] = execResultPy
    return outputDict

