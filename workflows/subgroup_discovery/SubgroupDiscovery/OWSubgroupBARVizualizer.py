"""
<name>Subgroup BAR Vizualization</name>
<description>Subgroup vizualization with BAR representation</description>
<icon>icons/SubgroupBARVizualization.png</icon>
<priority>1010</priority>
"""

from OWWidget import *
import OWGUI
import sys
from Beam_SD import *
from PyQt4 import QtGui


class OWSubgroupBARVizualizer(OWWidget):

    def __init__(self, parent = None, signalManager = None, name = "SubgroupBARVizualizer"):
        OWWidget.__init__(self, parent, signalManager, name, TRUE)
        self.inputs = [("Subgroup Descriptions", SDRules, self.vizualize)]
        self.outputs = [("Subgroup Descriptions Subset", SDRules),("Examples", orange.ExampleTable)]


        #######################
        #control area
        self.edtRules = QtGui.QPlainTextEdit(self.controlArea)   # print rules in this multi line edit
        self.controlArea.layout().addWidget(self.edtRules)

        OWGUI.button(self.controlArea, self, "Send subset", callback=self.sendSubset)

        #######################
        # main area - start of content (right) area
        self.tabs = OWGUI.tabWidget(self.mainArea) 		#tabs = QTabWidget(self.mainArea)

        # list box BAR widget
        self.lbBarRules = QtGui.QListWidget(self.tabs)
        self.lbBarRules.setSelectionMode(2)   #QAbstractItemView::MultiSelection
        self.connect(self.lbBarRules, SIGNAL("selectionChanged()"), self.sendSubset)
        OWGUI.createTabPage (self.tabs,"Vizualization BAR", widgetToAdd = self.lbBarRules, canScroll = True)

        # description of the vizualization
        #tab = QtGui.QGroupBox(self)
        labeltxt = """
                    This widget provides a vizualization of subgroups in a
                    bar form. The first line shows the distribution of the
                    entire dataset. The green color symbolizes the number
                    of positive examples and the red color symbolizes the
                    number of negative examples. Each following line provides
                    a visualization of one subgroup. The green and red color
                    represent the part of positive and negative examples each
                    subgroup covers. The green and the red color together
                    represent the size of the subgroup.

                    It is possible to select a subset of the subgroups. That
                    could be an input to other vizualizations.
                    """
        labelDescription = QLabel ( labeltxt, self.tabs )
        OWGUI.createTabPage (self.tabs,"Vizualization description", widgetToAdd = labelDescription, canScroll = True)

        self.resize(700,400)

    def vizualize(self, subgroups):
        if subgroups:
            # number of positive and negative examples
            self.subgroups = subgroups
            self.calcRates()

            # call for vizualization
            self.vizualizeBAR()


            # debug only
            self.edtRules.appendPlainText(("i\ttpr\tfpr\trule"))
            for i in range(len(self.subgroups.FPR)):
                #print ("%2d\t%0.2f\t%0.2f\t%s")%(i, self.TPR[i], self.FPR[i],self.subgroups.rules[i].ruleToString() )
                self.edtRules.appendPlainText(("%2d\t%0.2f\t%0.2f\t%s")%(i, self.subgroups.TPR[i], self.subgroups.FPR[i],self.subgroups.rules[i].ruleToString() ) )
        else:
            self.lbBarRules.clear()


    def calcRates(self):
        self.subgroups.sortByConf()  # sort by confidence
        self.subgroups.TPR = []
        self.subgroups.FPR = []
        self.P = len(self.subgroups.targetClassRule.TP) * 1.0  # number of all positive examples as a float
        self.N = len(self.subgroups.targetClassRule.FP) * 1.0  # number of all negative examples as a float
        for rule in self.subgroups.rules:
            self.subgroups.TPR.append( len(rule.TP) / self.P )  # true positive rate for this rule
            self.subgroups.FPR.append( len(rule.FP) / self.N )  # false positive example for this rule

    def vizualizeBAR(self):
        #the default rule
        self.lbBarRules.clear()
        self.lbBarRules.setIconSize(QSize(200,20))
        pix = self.createPixMap(1.0, 1.0)
       # self.lbBarRules.addItem(pix,self.subgroups.targetClassRule.ruleToString())
        icon = QtGui.QIcon(pix)
        QtGui.QListWidgetItem ( icon, self.subgroups.targetClassRule.ruleToString(), self.lbBarRules)

        # other rules
        for i in range(len(self.subgroups.rules)):
            pix = self.createPixMap(self.subgroups.TPR[i], self.subgroups.FPR[i])
            icon = QtGui.QIcon(pix)
            QtGui.QListWidgetItem ( icon, self.subgroups.rules[i].ruleToString(), self.lbBarRules )

    def createPixMap(self, tpr, fpr):
        w = 200
        h=20
        str = QString ("tmp")
        pix = QPixmap ( w, h+1)

        p = QPainter ( pix )
        p.fillRect ( 0, 0, w, h, QBrush(QColor(240,240,245)) )

        #line in the middle
        p.setPen(  QColor(155,155,155))
        sredina = int( self.N /(self.N + self.P) * (w-1))
        p.drawLine ( sredina, 0 , sredina, h )

        w1= int(fpr*sredina)   #red
        p.fillRect ( sredina - w1, 0, w1, h , QBrush(QColor(255, 55,55)) )
        str.setNum(fpr,'f',2)
        p.drawText ( sredina /2-7, 15, str )

        w1= int(tpr*(w-sredina))  #green
        p.fillRect ( sredina+1, 0, w1, h , QBrush(QColor(5, 225, 55)) )
        str.setNum(tpr,'f',2)
        p.drawText (sredina+(w-sredina)/2-7, 15, str )

        p.end()
        return pix


    def sendSubset(self):
        list = []
        for i in range(1, self.lbBarRules.count() ):
            if  self.lbBarRules.item(i).isSelected():
                list.append(i-1)
                self.edtRules.appendPlainText(str(i-1))
        if len(list)==0:
            ex = None
        else:
            tmp = self.subgroups.rules[list[0]].examples
            for i in range (1, len(list)):
                tmp.extend(self.subgroups.rules[list[i]].examples)
            ex = orange.ExampleTable(tmp)
            ex.removeDuplicates()


        subset = self.subgroups.makeSelection(list)
        subset.name = "%s subset"%(self.subgroups)
##        print "___________________"
##        if ex:
##            for e in ex:
##                print e

        self.send("Subgroup Descriptions Subset", subset)
        self.send("Examples", ex)
        self.edtRules.appendPlainText("send")




if __name__=="__main__":
    appl = QApplication(sys.argv)
    ow = OWSubgroupBARVizualizer()
    #appl.setMainWidget(ow)
    ow.show()

    filename = "..\\..\\doc\\datasets\\lenses.tab"
    if 'linux' in sys.platform:
        filename= "/usr/doc/orange/datasets/lenses.tab"
    dataset = orange.ExampleTable(filename)


    learner = Beam_SD(  minSupport = 0.2, beamWidth = 5, g = 1)
    rules = learner (dataset , targetClass= "soft", num_of_rules=5)
    ow.vizualize(rules)
    appl.exec_()

