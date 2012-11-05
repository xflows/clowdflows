"""
<name>Subgroup ROC Vizualization</name>
<description>Subgroup vizualization in the ROC space</description>
<icon>icons/SubgroupROCVizualization.png</icon>
<priority>1020</priority>
"""

import sys
from OWWidget import *
import OWGUI
from Beam_SD import *
from Apriori_SD import *
from OWSubgroupROCVizualizerGraph import *
from PyQt4 import QtGui
from calcHull import *
#from qttable import *

white = QColor(255,255,255)


class OWSubgroupROCVizualizer(OWWidget):

    def __init__(self, parent = None, signalManager = None, name = "SubgroupROCVizualizer"):
        OWWidget.__init__(self, parent, signalManager, name, TRUE)
        self.inputs = [("Subgroup Descriptions", SDRules, self.getSubgroup,0), ("Subgroup Descriptions Subset", SDRules, self.vizualizeSubset)]
        self.outputs = []
        ## data
        self.subgroups = []
        self.scolor = QColor(23,60,130)     # for subset vizualization
        self.subset = None
   #     self.blockSelectionChanges = 0

        #######################
        #control area

##        self.clb = QListBox(self.controlArea)
 ##       self.clb.setSelectionMode(QListBox.Multi)
  ##      self.clb.setMinimumHeight(50)
   ##     self.connect(self.clb, SIGNAL("selectionChanged()"), self.subgroupsSelectionChanged)
        self.clb = OWGUI.listBox(self.controlArea, self, value = None, labels = None, box = 1, tooltip = "List of algorithm names on input", callback = self.subgroupsSelectionChanged, selectionMode = QListWidget.MultiSelection)


        #self.boxControlArea = QVBoxLayout(self.controlArea)
        self.edtRules = QtGui.QPlainTextEdit(self.controlArea)     # print rules in this multi line edit
        self.controlArea.layout().addWidget(self.edtRules)


        #######################
        # main area - start of content (right) area
        self.tabs = OWGUI.tabWidget(self.mainArea)

        # graph ROC widget
        self.graphROC = OWSubgroupROCVizualizerGraph(self.tabs, None)
        OWGUI.createTabPage (self.tabs,"Vizualization ROC", widgetToAdd = self.graphROC)

        self.connect(self.graphButton, SIGNAL("clicked()"), self.graphROC.saveToFile)

        # description of the vizualization
        labeltxt = """
                    This widget provides a vizualization of subgroups in the
                    ROC space. Each dot represents one rule. The position of
                    the dot represents how good the subgroup is in the relation
                    to the true positive / false positive covered examples.

                    The percentage of the positive examples covered by the rule
                    is represented by the distance from the x axes. The percentage
                    of negative examples covered by the rule is represented by
                    the distance form the y axes. Therefore the best subgroups are
                    located in the left upper corner of the roc space.

                    The convex hull is the line connecting the ...
                    """
        labelDescription = QLabel ( labeltxt, self.tabs )
        OWGUI.createTabPage (self.tabs,"Vizualization description", widgetToAdd = labelDescription, canScroll = True)


    def getSubgroup(self, subgroups, id=None):
        ids = [x[0] for x in self.subgroups]
        if not subgroups: # remove subgroups
            if id in ids:
                del self.subgroups[ids.index(id)] # remove subgroups with this id
            else:
                return    # there are no subgroups with this id
        else:
            if id in ids: # update (already seen sobgroups from this source)
                indx = ids.index(id)
                subgroups.isSelected = self.subgroups[indx][1].isSelected
                self.subgroups[indx] = (id, subgroups)
                calcRates(subgroups)   ######## calc rates
            else:       # add new subgroups
                subgroups.isSelected = True
                self.subgroups.append((id, subgroups))
                calcRates(subgroups)   ######## calc rates
        self.edtRules.appendPlainText("%d subgroups sets on input." % len(self.subgroups))
        self.updateclb()

    def updateclb(self):
        self.blockSelectionChanges = 1
        self.clb.clear()
        colors = ColorPaletteHSV(len(self.subgroups))
        for (i,lt) in enumerate(self.subgroups):
            s = lt[1]
            self.clb.addItem(QListWidgetItem(ColorPixmap(colors[i]), s.name))
            s.color = colors[i]
        self.blockSelectionChanges = 0
        self.clb.selectAll()
        print "updateclb"
        self.subgroupsSelectionChanged()

    def subgroupsSelectionChanged(self):
        self.edtRules.appendPlainText("selection changed")
  #      if self.blockSelectionChanges: return
  #      self.printSubgroups()
        #print self.subgroups
        for (i,lt) in enumerate(self.subgroups):
            #print i, lt[1].name, lt[1].isSelected, self.clb.item(i)
            if self.clb.item(i):
                lt[1].isSelected = self.clb.item(i).isSelected()
        self.vizualize()

    def vizualize(self):
        self.graphROC.clearGraph()
        for s in self.subgroups:
            #
            # FIXME: make a proper fix, as this is only a temporary fix for some strange cases when s[1] doesn't have a color attribute
            #
            if s[1].isSelected and hasattr(s[1], 'color'):
                self.edtRules.appendPlainText("%s is selected" % s[1].name)
                color_l = s[1].color.dark(150)
                #draw the convex hull on the canvas
                self.graphROC.addCurve("hull", color_l, color_l, 3, style = QwtPlotCurve.Lines,\
                               xData = s[1].hullFPR, yData = s[1].hullTPR)

                #draw dots on the canvas
                self.graphROC.addCurve("dots", color_l, s[1].color,  10, style = QwtPlotCurve.NoCurve,\
                                       symbol = QwtSymbol.Ellipse, xData =s[1].FPR, yData = s[1].TPR)

                self.graphROC.addCurve("dots",   white, s[1].color, 6, style = QwtPlotCurve.NoCurve,\
                                       symbol = QwtSymbol.Ellipse, xData =s[1].FPR, yData = s[1].TPR)
        self.graphROC.replot()

####################_______________________##### O L D
    def vizualizeSubset(self, subset):
        if subset:
            self.subset = subset
            #draw dots on the canvas
            calcRatesSubset(subset)
            self.graphROC.addCurve("dots", self.scolor, self.scolor, 6, style = QwtPlotCurve.NoCurve,\
                                   symbol = QwtSymbol.Ellipse, xData =self.subset.FPR, yData = self.subset.TPR)
            self.graphROC.replot()
        else:
            self.subset = None



if __name__=="__main__":
    appl = QApplication(sys.argv)
    ow = OWSubgroupROCVizualizer()
   # appl.setMainWidget(ow)
    ow.show()

    filename = "..\\..\\doc\\datasets\\lenses.tab"
    if 'linux' in sys.platform:
        filename = "/usr/doc/orange/datasets/lenses.tab"
    dataset = orange.ExampleTable(filename)

    learner = Beam_SD(  minSupport = 0.2, beamWidth = 8, g = 5)
    rules = learner (dataset , targetClass= "hard", num_of_rules =5)
    rules.name = "Beam_SD"
    rulesSubset = rules.makeSelection([1,4])
    ow.getSubgroup(rules,0)

    learner2 = Apriori_SD(  minSupport = 0.1)
    rules2 = learner2 (dataset , targetClass= "soft")
    rules2.name = "Apriori_SD"
    ow.getSubgroup(rules2, 1)



    ow.vizualizeSubset(rulesSubset)

    appl.exec_()

