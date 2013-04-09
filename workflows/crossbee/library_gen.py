# -----------------------------------------------------------------------------------------------------
# WARNING: THIS IS AUTOMATICALLY GENERATED FILE, DO NOT EDIT IT MANUALLY AS YOU MAY LOOSE YOUR CHANGES!
# -----------------------------------------------------------------------------------------------------

from import_dotnet import *
from serialization_utils import *

def crossbee_construct_standard_heurisitc(inputDict):
    _name = ToString(inputDict['name'])
    _heurisitcSpec = ToEnum(CrossBeeInterfaces.Heurisitcs.StandardHeurisitc.Specification, inputDict['heurisitcSpec'], CrossBeeInterfaces.Heurisitcs.StandardHeurisitc.Specification.random)
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
    _calc = ToEnum(CrossBeeInterfaces.Heurisitcs.CalculatedHeustistic.Calculation, inputDict['calc'], CrossBeeInterfaces.Heurisitcs.CalculatedHeustistic.Calculation.Sum)
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

def crossbee_load_outlier_heuristics(inputDict):
    _namePrefix = ToString(inputDict['namePrefix'])
    _specification = ToString(inputDict['specification'])
    _relative = ToBool(inputDict['relative'])
    execResult = CrossBeeIntf.LoadOutlierHeuristics(_namePrefix, _specification, _relative)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['newHeurisitcs'] = execResultPy
    return outputDict

def crossbee_outlier_heuristics_spec(inputDict):
    _heuristics = ToNetObj(inputDict['heuristics'])
    execResult = CrossBeeIntf.OutlierHeuristicsSpec(_heuristics)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['specification'] = execResultPy
    return outputDict

def crossbee_outlier_detection_via_cross_validation(inputDict):
    _csf = ToNetObj(inputDict['csf'])
    _ds = ToNetObj(inputDict['ds'])
    _repetitionCount = ToInt(inputDict['repetitionCount'])
    _outlierThreshold = ToInt(inputDict['outlierThreshold'])
    _numOfSets = ToInt(inputDict['numOfSets'])
    _random = ToBool(inputDict['random'])
    _useSeed = ToBool(inputDict['useSeed'])
    _randomSeed = ToInt(inputDict['randomSeed'])
    _outlierWeighting = ToEnum(CrossBeeInterfaces.CrossBeeIntf.OutlierWeighting, inputDict['outlierWeighting'], CrossBeeInterfaces.CrossBeeIntf.OutlierWeighting.RelativePercentage)
    execResult = CrossBeeIntf.OutlierDetectionViaCrossValidation(_csf, _ds, _repetitionCount, _outlierThreshold, _numOfSets, _random, _useSeed, _randomSeed, _outlierWeighting)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['out'] = execResultPy
    return outputDict

def crossbee_apply_heurisitcs(inputDict):
    _termDataset = ToNetObj(inputDict['termDataset'])
    _heuristics = ToNetObj(inputDict['heuristics'])
    execResult = CrossBeeIntf.ApplyHeurisitcs(_termDataset, _heuristics)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['heur'] = execResultPy
    return outputDict

def crossbee_select_heuristics(inputDict):
    _heuristics = ToNetObj(inputDict['heuristics'])
    execResult = CrossBeeIntf.SelectHeuristics(_heuristics)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['heuristics'] = execResultPy
    return outputDict

def crossbee_rank_terms(inputDict):
    _heuristics = ToNetObj(inputDict['heuristics'])
    execResult = CrossBeeIntf.RankTerms(_heuristics)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['table'] = execResultPy
    return outputDict

def crossbee_explore_in_crossbee(inputDict):
    _parsedDoc = ToNetObj(inputDict['parsedDoc'])
    _heuristics = ToNetObj(inputDict['heuristics'])
    _bterms = ToNetObj(inputDict['bterms'])
    execResult = CrossBeeIntf.ExploreInCrossbee(_parsedDoc, _heuristics, _bterms)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    return outputDict

def crossbee_get_roc_curves(inputDict):
    _heuristics = ToNetObj(inputDict['heuristics'])
    _bterms = ToNetObj(inputDict['bterms'])
    execResult = CrossBeeIntf.GetRocCurves(_heuristics, _bterms)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['roc'] = execResultPy
    return outputDict

def crossbee_display_roc_curves(inputDict):
    _roc = ToNetObj(inputDict['roc'])
    execResult = CrossBeeIntf.DisplayRocCurves(_roc)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    return outputDict

def crossbee_get_performance_measures(inputDict):
    _heuristics = ToNetObj(inputDict['heuristics'])
    _bterms = ToNetObj(inputDict['bterms'])
    execResult = CrossBeeIntf.GetPerformanceMeasures(_heuristics, _bterms)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['perf'] = execResultPy
    return outputDict

def crossbee_get_viper_measures(inputDict):
    _heuristics = ToNetObj(inputDict['heuristics'])
    _bterms = ToNetObj(inputDict['bterms'])
    execResult = CrossBeeIntf.GetViperMeasures(_heuristics, _bterms)
    execResultPy = ToPyObj(execResult)
    outputDict = {}
    outputDict['perf'] = execResultPy
    return outputDict

