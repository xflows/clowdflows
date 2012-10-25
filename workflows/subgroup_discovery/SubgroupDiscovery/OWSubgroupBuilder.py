"""
<name>Build Subgroups</name>
<description>Runs subgroup discovery algorithms</description>
<icon>icons/SubgroupBuilder.png</icon>
<priority>100</priority>
"""
#
# OWSubgroupBuilder.py
#

from OWWidget import *
import OWGUI
import sys
import os
from SD_learner_classifier import *

###########################################################################################################
###########################################################################################################

class OWSubgroupBuilder(OWWidget):

    settingsList = [ "algorithm", "min_support", "g", "beam_width", "min_conf", "k", "max_rules"]
    def __init__(self, parent=None, signalManager = None, name='SubgroupBuilder'):
        OWWidget.__init__(self, parent, signalManager, name)

        self.inputs = [("Examples", ExampleTable, self.cdata)]
        self.outputs = [("Subgroup Descriptions", SDRules), ("Learner", orange.Learner),("Classifier", orange.Classifier) ]



        # Settings
        algorithms = ["SD", "SD-Preprocess", "Apriori-SD", "CN2-SD"]
        self.algorithm = algorithms[0]
        self.min_support = 5;
        self.g = 5
        self.beam_width = 20
        self.min_conf = 80
        self.k = 5
        self.name = "Subgroup SD"
        self.max_rules = 0
        self.classes = ["No data"]
        self.data = None

        self.loadSettings()

        # GUI - controlArea (left)
        self.learnerName = OWGUI.lineEdit(self.controlArea, self, "name", tooltip="Name to be used by other widgets to identify the learner/classifier", box="Learner/classifier name")

        box = OWGUI.widgetBox(self.controlArea, "Options", addSpace = True)
        OWGUI.comboBox(box, self, "algorithm", box=None, label="Algorithm", labelWidth=None, orientation='horizontal', items=algorithms, tooltip="Choose an algorithem", sendSelectedValue = 1, callback = self.algorithmChanged)
        self.spin_support = OWGUI.spin(box, self, "min_support", 0, 100, 1, None, "Minimal support [%] ",labelWidth=200, orientation="horizontal", callback=self.settingChanged)
        self.spin_g       = OWGUI.spin(box, self, "g", 1, 1000, 1, None, "Generalization parameter ",labelWidth=200, orientation="horizontal",callback=self.settingChanged)
        self.spin_beam_width = OWGUI.spin(box, self, "beam_width", 3, 100, 1, None, "Beam width ",labelWidth=200, orientation="horizontal",callback=self.settingChanged)
        self.spin_conf    = OWGUI.spin(box, self, "min_conf", 0, 100, 1, None, "Minimal confidence [%]",labelWidth=200, orientation="horizontal",callback=self.settingChanged)
        self.spin_k       = OWGUI.spin(box, self, "k", 3, 100, 1, None, "k - num of times covered before removed ", labelWidth=200,orientation="horizontal",callback=self.settingChanged)
        self.spin_max_rules = OWGUI.spin(box, self, "max_rules", 0, 20, 1, None, "Max. number of subgroups (0=no limitations) ", labelWidth=200,orientation="horizontal",callback=self.settingChanged)
        self.algorithmChanged()

        OWGUI.separator(self.controlArea)

        #target Class
        box = OWGUI.widgetBox(self.controlArea, "Target class", addSpace = True)
        self.listTargetValue = OWGUI.comboBox(box, self, "classes", box=None, label="Target class", labelWidth=None, orientation='horizontal', items=self.classes, tooltip="Choose the target class", sendSelectedValue = 1, callback = self.targetClassChanged)
        #self.lblTargetValue = QLabel("Target value:", self.controlArea)
        #self.listTargetValue = QListWidget( self.controlArea )
        #self.listTargetValue.addItem("No data")
        #self.connect(self.listTargetValue, SIGNAL("selectionChanged()"), self.targetClassChanged)
        self.listTargetValue.blockSignals (true)  # block signals until trehe are no rules to send

        OWGUI.separator(self.controlArea)

        OWGUI.button(self.controlArea, self, "Build subgroups", callback=self.build)
        OWGUI.button(self.controlArea, self, "Save ruleset to file in PMML format ...", callback=self.printToPmml)

        self.learner = SD_learner(name=self.name, algorithm=self.algorithm ,minSupport = self.min_support/100.0, minConfidence=self.min_conf/100.0 , beamWidth = self.beam_width, g = self.g, k=self.k, max_rules =self.max_rules)
        self.classifier = None
        self.subgroups = None

        # GUI -  main area (right)
        self.edtRules = QTextEdit(self.mainArea)                # the reules are printed in this text edit
        self.mainArea.layout().addWidget(self.edtRules)

        self.edtRules.setReadOnly(TRUE)
        OWGUI.rubber(box)




        self.resize(700,450)

    def algorithmChanged(self):
        """Set which setting are enabled and disabled for each algorithm and set learner."""
        if self.algorithm == "SD" or self.algorithm == "SD-Preprocess":
            self.spin_support.setDisabled(0)
            self.spin_g.setDisabled(0)
            self.spin_beam_width.setDisabled(0)
            self.spin_conf.setDisabled(1)
            self.spin_k.setDisabled(1)
        elif self.algorithm == "Apriori-SD":
            self.spin_support.setDisabled(0)
            self.spin_g.setDisabled(1)
            self.spin_beam_width.setDisabled(1)
            self.spin_conf.setDisabled(0)
            self.spin_k.setDisabled(0)
        else:
            self.spin_support.setDisabled(1)
            self.spin_g.setDisabled(1)
            self.spin_beam_width.setDisabled(1)
            self.spin_conf.setDisabled(1)
            self.spin_k.setDisabled(0)
        self.settingChanged()

    def settingChanged(self):
        self.learner = SD_learner(name=self.name, algorithm=self.algorithm ,minSupport = self.min_support/100.0, minConfidence=self.min_conf/100.0 , beamWidth = self.beam_width, g = self.g, k=self.k, max_rules =self.max_rules)
        self.learner.name = self.name
        self.send("Learner", self.learner)

    def cdata(self, dataset):
        self.classifier = None
        self.subgroups = None
        self.send("Subgroup Descriptions", self.subgroups)
        self.send("Classifier", self.classifier)
        self.listTargetValue.clear()

        self.data = self.isDataWithClass(dataset, orange.VarTypes.Discrete) and dataset or None

        if self.data:
            #fill combo box with all possible target class values
            self.data = dataset
            valueObj= self.data.domain.classVar.firstvalue()
            while valueObj:
                self.listTargetValue.addItem(valueObj.value)
                valueObj = self.data.domain.classVar.nextvalue(valueObj)
            self.listTargetValue.setCurrentIndex(0)
        else:
            self.listTargetValue.addItem('No data')
            self.listTargetValue.blockSignals (true)


    def build(self):
        if not self.data:
            return

        self.classifier = self.learner(self.data)
        self.edtRules.append(" %s %s supp=%d conf=%d bw=%0.2f g=%d k=%d rules=%d"%(self.name, self.algorithm , self.min_support, self.min_conf , self.beam_width,  self.g, self.k, self.max_rules))

        # send classifier

        self.classifier.name = self.name
        self.send("Classifier", self.classifier)

        self.listTargetValue.blockSignals ( false)  # enable signals, enable sending rules to other widgets
        if self.listTargetValue.currentIndex() <> -1:
            self.subgroups = self.classifier.getRules(targetClass= str(self.listTargetValue.currentText() ))
            self.subgroups.name = self.name
            self.send("Subgroup Descriptions", self.subgroups)

        # debug
        for rule in self.subgroups.rules:
            self.edtRules.append(rule.ruleToString())
        self.edtRules.append("----------------------------------")

    def printToPmml(self):
        if self.classifier == None:
            self.build()
        self.browseFile()
        outfile = open(self.filename, 'w')
        self.classifier.toPMML(outfile)
        outfile.close()


    def browseFile(self):
        tmpfilename = str(QFileDialog.getSaveFileName(self, 'Save rules as XML', os.getcwd(), 'XML file (*.xml)'))
        if os.path.splitext(tmpfilename)[1].lower() != '.xml':
            tmpfilename += '.xml'
        self.filename = tmpfilename


    def targetClassChanged(self):
        if self.classifier:
            self.subgroups = self.classifier.getRules(targetClass= str(self.listTargetValue.currentText()))
            self.edtRules.append(self.listTargetValue.currentText())
            self.subgroups.name = self.name
            self.send("Subgroup Descriptions", self.subgroups)

            # debug
            self.edtRules.append("target class changed")
            if self.subgroups:
                for rule in self.subgroups.rules:
                    self.edtRules.append(rule.ruleToString())
                self.edtRules.append("----------------------------------")


if __name__=="__main__":
    appl = QApplication(sys.argv)
    ow = OWSubgroupBuilder()
    #appl.setMainWidget(ow)
    ow.show()

    filename = "..\\..\\doc\\datasets\\lenses.tab"
    if 'linux' in sys.platform:
        filename= "/usr/doc/orange/datasets/lenses.tab"
    dataset = orange.ExampleTable(filename)

    ow.cdata(dataset)
    ow.saveSettings()
    appl.exec_()

