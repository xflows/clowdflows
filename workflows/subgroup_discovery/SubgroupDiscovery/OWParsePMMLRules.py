"""
<name>PMML Rule Parser</name>
<description>Parses the given PMML XML string and returns the rules as a SDRules object.</description>
<icon></icon>
<priority>100</priority>
<contact>Anze Vavpetic (anze.vavpetic@ijs.si)</contact>
"""
import sys
import orange
from SDRule import SDRule, SDRules
from Beam_SD import Beam_SD
import xml.dom.minidom as xml
from xml.dom import NotSupportedErr

from PyQt4.Qt import * 
from PyQt4.QtGui import *
from PyQt4.QtCore import *
from PyQt4.Qwt5 import *

from webServices import guiTools
try:
    from OWBaseWidget import OWBaseWidget
except ImportError:
    OWBaseWidget = guiTools.importOWBaseWidget()

class OWParsePMMLRules(OWBaseWidget):
    def __init__(self, parent=None, signalManager = None, name='ParsePMMLRules'):
        super(OWParsePMMLRules, self).__init__(parent, signalManager, name)
        
        self.inputs = [("Examples", orange.ExampleTable, self.getExamples), ("RulesAsPMML", str, self.getPMML)]
        self.outputs = [("Subgroup Descriptions", SDRules)]
        
        self.pmml = None
        self.data = None
        
        self.init()
        
    def init(self):
        self.gridlayout = QGridLayout(self)
        self.label = QLabel()
        self.label.setText("This widget has no parameters.")
        self.gridlayout.addWidget(self.label, 0, 0)
    
    def getExamples(self, data):
        self.data = data
        self.parse()
        
    def getPMML(self, xml):
        self.pmml = xml
        self.parse()
        
    def parse(self):
        if not self.pmml or not self.data:
            return
        
        rules = self.toSDRules(self.pmml, self.data)
        self.send('Subgroup Descriptions', rules)
    
    def toSDRules(self, pmml, data):
        '''
        Converts the PMML XML to SDRules.
        '''
        rules = []
        dom = xml.parseString(pmml)
        
        alg = dom.getElementsByTagName('RuleSetModel')[0].getAttribute('algorithmName')
        
        for node in dom.getElementsByTagName('SimpleRule'):
            rules.append(self.parseRule(node, data))
        
        targetClassRule = SDRule(data, rules[0].targetClass, conditions=[])
        sdrules = SDRules(rules, targetClassRule, algorithmName=alg)
        sdrules.name = alg
        
        return sdrules
        
    def parseRule(self, node, data):
        '''
        Handles the parsing of one rule element.
        '''
        # Check if the compound predicate uses the AND operator
        compound = node.getElementsByTagName('CompoundPredicate')
        if compound:
            for pred in compound:
                op = pred.getAttribute('booleanOperator')
                if op.lower() != 'and':
                    raise NotSupportedErr('Operator %s is not supported.' % op)
            
        targetClass = orange.Value(data.domain.classVar, str(node.getAttribute('score')))
        
        conditions = []
        for pred in node.getElementsByTagName('SimplePredicate'):
            attribute = str(pred.getAttribute('field'))
            attIdx = data.domain.index(attribute)
            parseVal = str(pred.getAttribute('value'))
            value = float(parseVal) if data.domain[attIdx].varType == orange.VarTypes.Continuous else parseVal
            
            conditions.append(orange.ValueFilter_discrete(position = attIdx, values = [orange.Value(data.domain[attIdx], value)]))
            
        return SDRule(data, targetClass, conditions=conditions)
    
if __name__ == '__main__':
    data = orange.ExampleTable('lenses')
    learner = Beam_SD(minSupport = 0.2, beamWidth = 5, g = 6)
    targetClass= orange.Value(data.domain.classVar, "soft")
    rules = learner(data, targetClass=targetClass, num_of_rules=3)
    pmmlRules = rules.toPMML().getvalue()
    a = QApplication(sys.argv)
    w = OWParsePMMLRules()
    w.getExamples(data)
    w.getPMML(pmmlRules)
    w.show()
    a.exec_()
    rules.printRules()