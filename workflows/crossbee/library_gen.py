# -----------------------------------------------------------------------------------------------------
# WARNING: THIS IS AUTOMATICALLY GENERATED FILE, DO NOT EDIT IT MANUALLY AS YOU MAY LOOSE YOUR CHANGES!
# -----------------------------------------------------------------------------------------------------

from import_dotnet import *
from serialization_utils import *

def crossbee_get_vocabulary(inputDict):
    _bow = ToNetObj(inputDict['bow'])
    _startIndex = ToInt(inputDict['startIndex'])
    _maxWords = ToInt(inputDict['maxWords'])
    execResult = CrossBeeIntf.GetVocabulary(_bow, _startIndex, _maxWords)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['vocabulary'] = execResultPy
    return outputDict

def crossbee_construct_standard_heurisitc(inputDict):
    _name = ToString(inputDict['name'])
    _heurisitcSpec = ToEnum(CrossBeeInterfaces.StandardHeurisitc.Specification, inputDict['heurisitcSpec'], CrossBeeInterfaces.StandardHeurisitc.Specification.random)
    execResult = CrossBeeIntf.ConstructStandardHeurisitc(_name, _heurisitcSpec)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['heurisitc'] = execResultPy
    return outputDict

def crossbee_construct_all_standard_heurisitc(inputDict):
    execResult = CrossBeeIntf.ConstructAllStandardHeurisitc()
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['heurisitcs'] = execResultPy
    return outputDict

def crossbee_construct_outlier_heuristics(inputDict):
    _name = ToString(inputDict['name'])
    _relative = ToBool(inputDict['relative'])
    _outlierDocumentIndexes = ToNetObj(inputDict['outlierDocumentIndexes'])
    execResult = CrossBeeIntf.ConstructOutlierHeuristics(_name, _relative, _outlierDocumentIndexes)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['newHeurisitcs'] = execResultPy
    return outputDict

def crossbee_construct_calculated_heuristics(inputDict):
    _name = ToString(inputDict['name'])
    _calc = ToEnum(CrossBeeInterfaces.CalculatedHeustistic.Calculation, inputDict['calc'], CrossBeeInterfaces.CalculatedHeustistic.Calculation.Sum)
    _heuristics = ToNetObj(inputDict['heuristics'])
    execResult = CrossBeeIntf.ConstructCalculatedHeuristics(_name, _calc, _heuristics)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['newHeurisitcs'] = execResultPy
    return outputDict

def crossbee_construct_ensemble_heuristics(inputDict):
    _name = ToString(inputDict['name'])
    _heuristics = ToNetObj(inputDict['heuristics'])
    execResult = CrossBeeIntf.ConstructEnsembleHeuristics(_name, _heuristics)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['newHeurisitcs'] = execResultPy
    return outputDict

def crossbee_combine_heuristics(inputDict):
    _heuristics = ToNetObj(inputDict['heuristics'])
    execResult = CrossBeeIntf.CombineHeuristics(_heuristics)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['newHeurisitcs'] = execResultPy
    return outputDict

def crossbee_get_heuristic_names(inputDict):
    _heuristics = ToNetObj(inputDict['heuristics'])
    execResult = CrossBeeIntf.GetHeuristicNames(_heuristics)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['names'] = execResultPy
    return outputDict

def crossbee_get_heuristic_structure(inputDict):
    _heuristics = ToNetObj(inputDict['heuristics'])
    execResult = CrossBeeIntf.GetHeuristicStructure(_heuristics)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['structure'] = execResultPy
    return outputDict

