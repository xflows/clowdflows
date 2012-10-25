"""
<name>Subgroup Evaluation</name>
<description>Evaluates subgroup discovery algorithms with evaluation measures.</description>
<icon>icons/SubgroupEvaluation.png</icon>
<priority>200</priority>
"""
#
# OWSubgroupEvaluation.py
#

from PyQt4 import QtGui
from OWWidget import *
import OWGUI
from SD_learner_classifier import *
from math import log
from Beam_SD import *
from Apriori_SD import *
from CN2_SD import *
###########################################################################################################
###########################################################################################################

class OWSubgroupEvaluation(OWWidget):
    settingsList = [ "nFolds", "stdevBtnStatus"]

    eval_measures = [ ('Average coverage',         '  avgCOV     ', 'Coverage'),
                      ('Target support',           '  SUP        ', 'Support'),
                      ('Average ruleset size',     '  SIZE       ', 'Size'),
                      ('Average complexity',       ' COMPLEX   ', 'Complexity'),
                      ('Average rule significance','  SIG        ', 'Significance'),
                      ('Average rule unusualness', '  avgWRACC ', 'Unusualness'),
                      ('Classification accuracy',  '  CA         ', 'Classification accuracy'),
                      ('Area under ROC',           '  AUC        ', 'AUC') ]

    def __init__(self, parent = None, signalManager = None):
        OWWidget.__init__(self, parent, signalManager, 'SubgroupEvaluation')

        # defining widget input ===========================================================================
        self.inputs = [('Data', ExampleTable, self.setData, Default), ('Learner', orange.Learner, self.setLearner, Multiple)]

        # Settings  =======================================================================================
        self.nFolds = 10                # cross validation folds
        self.stdevBtnStatus = 0
        self.usedMeasure = [1]*len(self.eval_measures)
       # self.coverage = 1
       # self.support  = 1
       # self.size = 1
       # self.complexity = 1
       # self.significance = 1
       # self.unusualness = 1
       # self.classification_accuracy = 1
       # self.auc = 1

        self.loadSettings()


        self.data = None                # input data set
        self.learndata = None           # separate learn data set
        self.testdata = None            # separate test data set
        self.learners = []              # set of learners (input)
        self.currentTargetClass = None  # current selection of target class
        self.builtClassifiers = []      # self.builtClassifiers => [ ( learner_id, [temp = list_of_classifiers] ) ]
        self.scores = dict()            # to be displayed in the table
        self.stdev = dict()

        self.selectedValue = []
        self.targetValues =[]

        # Logging
        self.logging = False
        self.log_string = ''
        self.log = dict()


        # GUI   ===========================================================================================
        # control area  ===================================================================================
        self.sBox = OWGUI.widgetBox(self.controlArea, 'Cross validation')
        self.spinBox = OWGUI.spin(self.sBox, self, "nFolds", 2, 20, step=1, box="Number of folds:", tooltip="Number of folds for cross validation\n (usually 10)", callback=self.spinBoxChanged)

        self.applyBtn = OWGUI.button(self.sBox, self, "&Apply", self.buttonPressed)
        self.applyBtn.setDisabled(True)

        # select target Class
        targetClassBox = OWGUI.widgetBox(self.controlArea, 'Target class:')
        self.listTargetValue = OWGUI.listBox(targetClassBox, self, "selectedValue", "targetValues", callback = self.targetClassChanged)

        self.listTargetValue.addItem('No data')


        # evaluation measures
##        self.evalMeasureBox = OWGUI.widgetBox(self.controlArea, 'Evaluation measures')
##        self.evalMeasureBtn = []
       # self.evalMeasureChoosen = [None]*len(self.eval_measures)
       # for i in range(len(self.eval_measures)):
       #     self.evalMeasureChoosen[i] = lambda value, id=i: self.measureStateChanged(value, id)   # ugly hack

##       OWGUI.checkBox(self.evalMeasureBox, self, "coverage", self.eval_measures[0][0], callback=self.measureStateChanged(self.coverage, 0) )
##        OWGUI.checkBox(self.evalMeasureBox, self, "support", self.eval_measures[1][0], callback=self.measureStateChanged(self.support, 1) )
##        OWGUI.checkBox(self.evalMeasureBox, self, "size", self.eval_measures[2][0], callback=self.measureStateChanged(self.size, 2) )
##        OWGUI.checkBox(self.evalMeasureBox, self, "complexity", self.eval_measures[3][0], callback=self.measureStateChanged(self.complexity, 3) )
##        OWGUI.checkBox(self.evalMeasureBox, self, "significance", self.eval_measures[4][0], callback=self.measureStateChanged(self.significance, 4) )
##        OWGUI.checkBox(self.evalMeasureBox, self, "unusualness", self.eval_measures[5][0],callback=self.measureStateChanged(self.unusualness, 5) )
##        OWGUI.checkBox(self.evalMeasureBox, self, "classification_accuracy", self.eval_measures[6][0], callback=self.measureStateChanged(self.classification_accuracy, 6) )
##        OWGUI.checkBox(self.evalMeasureBox, self, "auc", self.eval_measures[7][0], callback=self.measureStateChanged(self.auc, 7) )


        # show standard deviation
        self.showStdev = OWGUI.widgetBox(self.controlArea, 'Standard deviation')
        self.showStdevButton = OWGUI.checkBox(self.showStdev,self,"stdevBtnStatus","Show standard deviation",callback=self.paintscores)
        OWGUI.separator(self.controlArea)

        # status print
        self.statusBox = OWGUI.widgetBox(self.controlArea, 'Status')
        self.status = OWGUI.label(self.statusBox, self, "")


        # main area (right)    ============================================================================
        #self.layout = QVBoxLayout(self.mainArea)
        tabs = OWGUI.tabWidget(self.mainArea)

        # two tabs: one for the actual results and one for the explanation of the results and information about the authors

        # tab 1 - evaluation results    ===================================================================
        self.tab1 = OWGUI.widgetBox(self)
        self.resultsTable = OWGUI.table(self.tab1)
        ##self.resultsTable.setSelectionMode(QTable.NoSelection)

        # Information   ===================================================================================
        self.infoSection = OWGUI.widgetBox(self.tab1, 'Information')
        self.infoSize = OWGUI.label(self.infoSection,self,'Input status: No data on input, no learner(s) on input')
        self.infoDistribution = OWGUI.label(self.infoSection,self," ")
        self.infoClassDistribution = OWGUI.label(self.infoSection,self," ")

        ##tabs.insertTab(self.tab1, 'Evaluation results ')
        OWGUI.createTabPage (tabs,"Evaluation results", widgetToAdd = self.tab1, canScroll = True)

        # tab 2 - help about    ===========================================================================
        ##tab2 = OWGUI.widgetBox(self)
        labeltxt = '''
                Subgroup discovery evaluation

                This widget performs evaluation of discovered subgroups with evaluation measures described in tha paper
                B. Kavsek and N. Lavrac. APRIORI-SD: Adapting association rule learning to subgroup discovery. Applied Artificial Intelligence, 20(7):543-583, 2006.

                The abbreviations used in this widget denote the following measures:
                       avgCOV  \t Average coverage
                       SUP     \t Target support
                       SIZE    \t Average ruleset size
                       COMPLEX \t Average complexity
                       SIG     \t Average rule significance
                       avgWRACC\t Average rule unusualness
                       CA      \t Classification accuracy
                       AUC     \t Area under ROC curve

                                Authors:
                                        Miha Rojko and Robert Ravnik

                                Superviseb by
                                        Petra Kralj, B.Sc.
                                        Department of Knowledge Technologies, Jozef Stefan Institute
                                        Ljubljana, Slovenia

                version 0.2
                date: 15.10.2010
                '''

        labelDescription = QLabel (labeltxt, tabs)
        ##tabs.insertTab(tab2, 'Help about   ')
        OWGUI.createTabPage (tabs,"Vizualization description", widgetToAdd = labelDescription, canScroll = True)

        # final adding on self.mainArea     ===============================================================
        ##self.layout.add(tabs)

        # signals   =======================================================================================
        #self.connect(self.spinBox, SIGNAL('valueChanged(int)'), self.spinBoxChanged)



       # self.connect(self.showStdevButton, SIGNAL('toggled(bool)'), self.stdevChanged)

        #   ===============================================================================================
        self.resize(880,420)

###########################################################################################################
# FUNCTIONS

# ... to process widget input   ===========================================================================

    # change contents of targetValueList and prints data input properties
    def setData(self, data):

        self.log['header'] = 'Number of folds: '+str(self.nFolds)

        # clear print in Target class box
        self.listTargetValue.clear()
        # clear print in Information box
        self.infoDistribution.setText('')
        self.infoClassDistribution.setText('')

        #self.currentTargetClass = None

        # data OK?
#        if data and not data.domain.hasContinuousAttributes() and data.domain.hasDiscreteAttributes():
        if data and data.domain.hasDiscreteAttributes():
            # save new data
            self.data = data
            self.status.setText('')
            self.scores = dict()
            self.currentTargetClass = None

        # when data on input
        if self.data:
            # create learning set and testing set
            self.learndata, self.testdata = self.sampleData(self.data, self.nFolds)
            #values = self.data.domain.classVar.values
            #for i in range(len(values)):
            #    self.listTargetValue.insertItem(values[i])
            #self.listTargetValue.setCurrentItem(0)

            # fill list box with all possible target class values
            self.targetValues = self.data.domain.classVar.values
            self.currentTargetClass = self.targetValues[0]
            self.selectedValue = [self.currentTargetClass]



            # build nFolds classifiers of this learners
            if len(self.learners):
                self.buildClassifiers(self.learners)

            # prints input properties in Information group
            # done because of currect print in infoSize lable of Information group  =======================
            string = str(self.infoSize.text())
            string_split = string.split(',')
            string_split[0] = 'Input status:  %s examples from data' %len(self.data)
            string = ','.join(string_split)
            self.infoSize.setText(string)
            # =============================================================================================

            self.infoDistribution.setText('Distribution of target class:  %s' %self.data.domain.classVar.name)
            classDistributionString = ''
            for i in range(len(self.data.domain.classVar.values)):
                if i == 0:
                    classDistributionString += '\t%s\t ... \t%.1f %%' %(self.data.domain.classVar.values[i], 100*(orange.getClassDistribution(self.data)[i]/len(self.data)))
                else:
                    classDistributionString += '\n\t%s\t ... \t%.1f %%' %(self.data.domain.classVar.values[i], 100*(orange.getClassDistribution(self.data)[i]/len(self.data)))
            self.infoClassDistribution.setText(classDistributionString)

        # when no data on input
        else:
            # done because of currect print in infoSize lable of Information group  =======================
            string = str(self.infoSize.text())
            string_split = string.split(',')
            string_split[0] = 'Input status: No data on input'
            string = ','.join(string_split)
            self.infoSize.setText(string)
            # =============================================================================================
            # if no target class ...
            self.listTargetValue.insertItem('No data')
            self.applyBtn.setDisabled(True)
            self.status.setText('')
            # erase resultsTable
            self.resultsTable.setNumRows(0)
            self.resultsTable.setNumCols(0)

    # goes through all input learners
    def setLearner(self, learner, id = None):
        # learners id already on input
        ids = [l[0] for l in self.learners]

        # remove a learner and corresponding results
        if not learner and ids.count(id):
            # delete learner from list of learners
            del self.learners[ids.index(id)]
            self.scores.pop(id)
            self.paintscores()
        else:
            # update (already seen a learner from this source)
            if ids.count(id):
                indx = ids.index(id)
                self.learners[indx] = (id, learner)
            # add new learner
            else:
                self.learners.append((id, learner))
            # build nFolds classifiers of this learner
            if self.data:
                self.buildClassifiers([(id, learner)])

        # if any learner(s) on input
        if len(self.learners):
            # done because of currect print in infoSize lable of Information group  =======================
            string = str(self.infoSize.text())
            string_split = string.split(',')
            string_split[1] = ' %d learner(s) on input' % len(self.learners)
            string = ','.join(string_split)
            self.infoSize.setText(string)
            # =============================================================================================
        # if no learner(s) on input
        else:
            # done because of currect print in infoSize lable of Information group  =======================
            string = str(self.infoSize.text())
            string_split = string.split(',')
            string_split[1] = ' no learner(s) on input'
            string = ','.join(string_split)
            self.infoSize.setText(string)
            # =============================================================================================
            self.applyBtn.setDisabled(True)
            # erase resultsTable
            self.resultsTable.setNumRows(0)
            self.resultsTable.setNumCols(0)

# =========================================================================================================
# ... to process actions in widget GUI  ===================================================================

    # prints in Status label notification to press apply button
    def spinBoxChanged(self):
        # if input is OK and newValue is different than old one (nFolds)
        #if self.data and len(self.learners) and newValue != self.nFolds:
        #    self.status.setText('To apply changes press Apply!')
        #    self.applyBtn.setEnabled(True)
        #elif self.data and len(self.learners) and newValue:
        #    self.status.setText('')
        #    self.applyBtn.setDisabled(True)
        # save the number of folds
        #self.nFolds = newValue
        self.applyBtn.setEnabled(True)

    # just forward the request to start calculating
    def buttonPressed(self):
        # create learning set and testing set
        self.learndata, self.testdata = self.sampleData(self.data, self.nFolds)
        # clear list of classifiers
        #print 'update vseh learnerja'
        for l in self.learners:
            l[1].classifiers = []
        # build nFolds classifiers of this learner
        self.buildClassifiers(self.learners)

    # saves current target class and calls for change of table print
    def targetClassChanged(self):
        self.currentTargetClass = self.listTargetValue.currentItem().text()
        # update resultsTable
        self.paintscores()

    # change visibility of columns in table when checkbox in Evaluation measures group is checked or unchecked
    def measureStateChanged(self, value, id):
        self.usedMeasure[id] = value

        self.paintscores()


# =========================================================================================================
# ... to process all other stuff

    # devide input data for cross validation
    def sampleData(self, data, nFolds):
        train = []
        test = []
        indeces = orange.MakeRandomIndicesCV(data, nFolds, randseed = False, stratified = orange.MakeRandomIndices.StratifiedIfPossible)
        for fold in range(nFolds):
            train.append(data.select(indeces, fold, negate = 1))
            test.append(data.select(indeces, fold))

            '''# print in log file
            self.log['nFolds_'+str(fold)] = 'Fold '+str(fold)+':'
            self.log['trainData_'+str(fold)] = 'Train data:\n'
            self.log['testData_'+str(fold)] = 'Test data:\n'
            self.log['trainData_'+str(fold)] += '\t'.join([str(d[1]) for d in data.domain])+'\n'
            self.log['testData_'+str(fold)] += '\t'.join([str(d[1]) for d in data.domain])+'\n'
            for d_1 in data.select(indeces, fold, negate = 1):
                self.log['trainData_'+str(fold)] += '\t'.join([str(d) for d in d_1])+'\n'
            for d_0 in data.select(indeces, fold, negate = 0):
                self.log['testData_'+str(fold)] += '\t'.join([str(d) for d in d_0])+'\n' '''

        return train, test

    # building nFolds classifiers for every learner
    def buildClassifiers(self, learners):
        # clear print in Status label (to press Apply button)
        self.status.clear()
        # disable apply button
        self.applyBtn.setDisabled(True)

        pb = OWGUI.ProgressBar(self, iterations=self.nFolds*len(learners))
        temp = []
        for learner in learners:
            for i in range(len(self.learndata)):
                temp.append(learner[1](self.learndata[i]))
                pb.advance()
            learner[1].classifiers = temp
            temp = []
        pb.finish()
        # calculate its measure evaluation
        self.score(learners)
        self.paintscores()

    # calculating evaluating measures
    def score(self, learners):

        def calcHull(subgroups, Y, X, A, B):
            #inicialization
            C = (-1,-1)    # best new point point
            y = -1         # best distance

            # calculate best new point
            if (B[0]-A[0]) != 0:
                k = (B[1]-A[1]) / (B[0]-A[0])  # coefficient of the line between A and B
                for i in range(len(Y)):        # check every point
                    yn = Y[i] -( k * ( X[i] - A[0] ) + A[1])   # vertical distance between point i and line AB
                    if yn>0 and yn > y:        # if new distance is the greatest so far
                        C = (X[i], Y[i])       # the new point is the best so far
                        y = yn

            # if new point on the hull was found
            if C != (-1,-1):
                # recursivey call this function on the LEFT side of the point
                Xl =[]
                Yl =[]
                for i in range(len(X)):
                    if X[i]>A[0] and X[i]<C[0]:
                        Xl.append(X[i])
                        Yl.append(Y[i])
                calcHull(subgroups, Yl, Xl, A,C)  # recursive call

                subgroups.hullTPR.append(C[1])
                subgroups.hullFPR.append(C[0])

                # recursivey call this function on the RIGHT side of the point
                Xu =[]
                Yu =[]
                for i in range(len(X)):
                    if X[i]>C[0] and X[i]<B[0]:
                        Xu.append(X[i])
                        Yu.append(Y[i])
                calcHull(subgroups, Yu, Xu, C,B)  # recursive call

        def calcAUC(X,Y):
            area = 0.0
            for i in range(len(X)-1):
                x = X[i+1]-X[i]
                y1 = Y[i]
                y2 = Y[i+1]
                trapez = x* (y1+y2)/2
                area = area + trapez
            return area

        # return the size of induced ruleset for one target class value (without default rule)
        def calculate(learner, ruleset, level):
            size = 0.0
            complexity = 0.0
            covarage = 0.0
            support = []
            significance = 0.0
            unusualness = 0.0
            accuracy = 0.0
            X = []
            Y = []
            learner.hullTPR = [0]
            learner.hullFPR = [0]
            TP_table = set()
            TN_table = set()

            stdev_list = []
            for i in range(len(self.eval_measures)):
                stdev_list.append([])

            if ruleset:
                N = len(self.testdata[level])
                temp = orange.getClassDistribution(self.testdata[level])
                Poz = temp[ruleset[0].targetClass]
                Neg = 0
                tempN = filter(lambda t: t != ruleset[0].targetClass, temp.keys())
                for t in tempN:
                    Neg += temp[t]

                # correct when varibale is 0
                if Poz == 0:
                    Poz = 1e-15
                if Neg == 0:
                    Neg = 1e-15

                for rule in ruleset:
                    # if we don't have dafault rule
                    if len(rule.filter.conditions):
                        TP = filter(lambda e: e.getclass() == rule.targetClass, rule.filter(self.testdata[level]))
                        FP = filter(lambda e: e.getclass() != rule.targetClass, rule.filter(self.testdata[level]))
                        TN = filter(lambda e: e.getclass() != rule.targetClass, rule.filter(self.testdata[level], negate = 1))
                        nX = len(TP) + len(FP)
                        nY = int(orange.getClassDistribution(self.testdata[level])[rule.targetClass])
                        nXY = len(TP)
                        cov = float(nX)/N

                        # correct when variable is 0
                        if nX == 0:
                            nX = 1e-15
                        if nY == 0:
                            nY = 1e-15
                        if nXY == 0:
                            nXY = 1e-15
                        if cov == 0:
                            cov = 1e-15

                        # stdev staff
                        stdev_list[0].append(cov)

                        #support_delta = 0.0
                        #for ex in set(TP):
                        #    if ex not in support:
                        #        support_delta += 1.0

                        for e in TP:
                            support.append(e)
                            TP_table.add(e)
                        for e in TN:
                            TN_table.add(e)

                        covarage += cov
                        size += 1.0
                        complexity += rule.complexity
                        if (float(nXY) / (nY * cov)) > 1.0:
                            significance += 2.0 * nXY * log( float(nXY) / (nY * cov) )
                        unusualness += cov * (float(nXY)/nX - float(nY)/N)
                        accuracy += (float(len(TP)) + float(len(TN)))/N
                        X.append(float(len(FP))/Neg)
                        Y.append(float(len(TP))/Poz)

                        # stdev staff
                        #stdev_list[1].append(float(support_delta)/Poz )
                        stdev_list[1].append(float(len(TP))/Poz)
                        stdev_list[2].append(1.0)
                        stdev_list[3].append(rule.complexity)
                        stdev_list[4].append( 2.0 * nXY * log( float(nXY)/(nY * cov) ))
                        stdev_list[5].append(cov * (float(nXY)/nX - float(nY)/N))

                        if self.logging:
                            # print to log file
                            self.log_string += '\n\t\t\tRule '+str(int(size))+': '+rule.ruleToString()
                            self.log_string += '\n\t\t\t\tTP = '+str(len(TP))
                            for tp in TP:
                                self.log_string += '\n\t\t\t\t\t'+str(tp).split(', {')[0]
                            self.log_string += '\n\t\t\t\tFP = '+str(len(FP))
                            for fp in FP:
                                self.log_string += '\n\t\t\t\t\t'+str(fp).split(', {')[0]
                            self.log_string += '\n\t\t\t\tTN = '+str(len(TN))
                            for tn in TN:
                                self.log_string += '\n\t\t\t\t\t'+str(tn).split(', {')[0]
                            self.log_string += '\n\t\t\t\tn(X) = '+str(int(nX))
                            self.log_string += '\n\t\t\t\tn(Y) = '+str(int(nY))
                            self.log_string += '\n\t\t\t\tn(XY) = '+str(int(nXY))
                            self.log_string += '\n\t\t\t\tN = '+str(N)
                            self.log_string += '\n\t\t\t\tPoz = '+str(int(Poz))
                            self.log_string += '\n\t\t\t\tNeg = '+str(int(Neg))
                            self.log_string += '\n\t\t\t\tCovarage = '+str(cov)
                            self.log_string += '\n\t\t\t\tComplexity = '+str(rule.complexity)
                            if (float(nXY) / (nY * cov)) > 1.0:
                                self.log_string += '\n\t\t\t\tSignificance = '+str(2.0 * nXY * log( float(nXY) / (nY * cov) ))
                            else:
                                self.log_string += '\n\t\t\t\tSignificance = '+str(0.0)
                            self.log_string += '\n\t\t\t\tUnusualness = '+str(cov * (float(nXY)/nX - float(nY)/N))

                calcHull(learner, Y, X , A=(0,0), B=(1,1))
                learner.hullTPR.append(1)
                learner.hullFPR.append(1)

                if size > 0:
                    stdev_list[6].append(float(float(accuracy)/size))
                else:
                    stdev_list[6].append(float("infinity"))
                stdev_list[7].append(calcAUC(learner.hullFPR, learner.hullTPR))

                if self.logging:
                    # print to log file
                    self.log_string += '\n\t\t\tSupport = '+str(len(set(support)))
                    self.log_string += '\n\t\t\tClassification accuracy = '+str(float(accuracy)/size)
                    self.log_string += '\n\t\t\tSize = '+str(int(size))
                    self.log_string += '\n\t\t\tTPr = '+str( [tpr for tpr in learner.hullTPR] )
                    self.log_string += '\n\t\t\tFPr = '+str( [fpr for fpr in learner.hullFPR] )
                    self.log_string += '\n\t\t\tAUC = '+str(float(calcAUC(learner.hullFPR, learner.hullTPR)))

                # dodano 1
                for i in [0,3,4,5,6]:
                    for j,elem in enumerate(stdev_list[i]):
                        stdev_list[i][j] /= size

                return float(size), float(complexity)/size, float(covarage)/size, \
                float(len(set(support)))/Poz, float(significance)/size, float(unusualness)/size, \
                float(float(accuracy)/size), float(calcAUC(learner.hullFPR, learner.hullTPR)), stdev_list
            # if we don't have induced rules
            else:
                return 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, stdev_list

        def calcStdev(list):
            if len(list):
                avg = sum(list)/len(list)
                sdsq = sum([(i - avg) ** 2 for i in list])
                return (sdsq / len(list)) ** .5
            return -1.0

################################################################################

        for learner in learners:
            id = learner[0]
            learner = learner[1]

            if self.logging:
                # print to log file
                if isinstance(learner.learner, Beam_SD):
                    self.log_string += '\n\n'+learner.name+', settings => minSupport = '+\
                    str(learner.learner.minSupport)+', beamWidth = '+str(learner.learner.beamWidth)+', g = '+str(learner.learner.g)
                elif isinstance(learner.learner, Apriori_SD):
                    self.log_string += '\n\n'+ learner.name+', settings => k: '+str(learner.max_rules)+', minSupport = '+\
                    str(learner.learner.minSup)+', minConf = '+str(learner.learner.minConf)
                elif isinstance(learner.learner, CN2_SD):
                    self.log_string += '\n\n'+ learner.name+', settings => k: '+str(learner.max_rules)

            # add new variables which will hold results
            learner.scores = dict()
            learner.stdev = dict()

            covarage = [0.0]*len(self.data.domain.classVar.values)
            support = [0.0]*len(self.data.domain.classVar.values)
            size = [0.0]*len(self.data.domain.classVar.values)
            complexity = [0.0]*len(self.data.domain.classVar.values)
            significance = [0.0]*len(self.data.domain.classVar.values)
            unusualness = [0.0]*len(self.data.domain.classVar.values)
            accuracy = [0.0]*len(self.data.domain.classVar.values)
            AUC = [0.0]*len(self.data.domain.classVar.values)

            stdev_list = []
            for i in range(len(self.testdata[0].domain.classVar.values)):
                stdev_list.append([])
                for x in range(len(self.eval_measures)):
                    stdev_list[i].append([])

            # needed just for print in log file
            classifier_index = 0

            for classifier in learner.classifiers:

                if self.logging:
                    # print to log file
                    self.log_string += '\n\tClassifier '+str(classifier_index+1)+':'
                    self.log_string += '\n\t\tTrain data (target class = '+self.learndata[classifier_index].domain.classVar.name+'):'
                    self.log_string += '\n\t\t\t'+str(self.learndata[classifier_index].domain)
                    for e in self.learndata[classifier_index]:
                        self.log_string += '\n\t\t\t'+str(e).split(', {')[0]
                    self.log_string += '\n\t\tTest data (target class = '+self.testdata[classifier_index].domain.classVar.name+'):'
                    self.log_string += '\n\t\t\t'+str(self.testdata[classifier_index].domain)
                    for e in self.testdata[classifier_index]:
                        self.log_string += '\n\t\t\t'+str(e).split(', {')[0]

                i = 0

                for rulesClass in classifier.rulesClass:
                    # set of induced rules for this target class value
                    ruleset = rulesClass.rules.rules

                    if self.logging:
                        # print to log file
                        if ruleset:
                            self.log_string += '\n\t\tTarget class value: '+ruleset[0].targetClass.value

                    # addting result in list
                    siz, comp, cov, sup, sig, \
                    unusual, acc, auc, stdev = calculate(learner, ruleset, classifier_index)
                    # concatenate stdev lists
                    for j,measure in enumerate(stdev_list[i]):
                        stdev_list[i][j] += stdev[j]

                    covarage[i] += cov/self.nFolds
                    support[i] += sup/self.nFolds
                    size[i] += siz/self.nFolds
                    complexity[i] += comp/self.nFolds
                    significance[i] += sig/self.nFolds
                    unusualness[i] += unusual/self.nFolds
                    accuracy[i] += acc/self.nFolds
                    AUC[i] += auc/self.nFolds

                    i+= 1

                if self.logging:
                    # print to log
                    j = 0
                    self.log_string += '\n\t\tCalculated measures for the classifier '+str(classifier_index+1)+' of the learner '+learner.name
                    for tc in self.testdata[0].domain.classVar.values:
                        self.log_string += '\n\t\t\tTarget class value: '+str(tc)
                        self.log_string += '\n\t\t\t\tCovarage = '+str(covarage[j])
                        self.log_string += '\n\t\t\t\tSupport = '+str(support[j])
                        self.log_string += '\n\t\t\t\tSize = '+str(size[j])
                        self.log_string += '\n\t\t\t\tComplexity = '+str(complexity[j])
                        self.log_string += '\n\t\t\t\tSignificance = '+str(significance[j])
                        self.log_string += '\n\t\t\t\tUnusualness = '+str(unusualness[j])
                        self.log_string += '\n\t\t\t\tAccuracy = '+str(accuracy[j])
                        self.log_string += '\n\t\t\t\tAUC = '+str(AUC[j])
                        j += 1

                classifier_index += 1

                    #print '______________________________________________________________'
            #print '______________________________________________________________'
            if self.logging:
                # print to log file
                self.log_string += '\n\n\tCalculated evaluation measures for learner '+learner.name

            # to proces standard deviation of the CA and support
            avg_stdev_accuracy = 0.0
            # dodano
            #avg_stdev_support = 0.0
            for tc_i in range(len(self.testdata[0].domain.classVar.values)):
                avg_stdev_accuracy += sum(stdev_list[tc_i][6])
                # dodano
                #avg_stdev_support += sum(stdev_list[tc_i][1])
            avg_stdev_accuracy /= len(self.testdata[0].domain.classVar.values)
            # dodano
            #avg_stdev_support /= len(self.testdata[0].domain.classVar.values)

            i = 0
            for tc in self.testdata[0].domain.classVar.values:
                stdev_out = []
                for measure in stdev_list[i]:
                    stdev_out.append(calcStdev(measure))

                # CA and support hack, CA and support must not change! :)
                avg_accuracy = 1.0 * sum(accuracy)/len(accuracy)
                stdev_out[6] = avg_stdev_accuracy
                # dodano
                #avg_support = 1.0 * sum(support)/len(support)
                #stdev_out[1] = avg_stdev_support
                #learner.scores[tc] = [covarage[i],avg_support,size[i],complexity[i],significance[i],unusualness[i],avg_accuracy,AUC[i]]
                learner.scores[tc] = [covarage[i],support[i],size[i],complexity[i],significance[i],unusualness[i],avg_accuracy,AUC[i]]
                learner.stdev[tc] = stdev_out

                # print to log file
                if self.logging:
                    self.log_string += '\n\t\tTarget class value: '+str(tc)
                    scores = learner.scores[tc]
                    for score in range(len(scores)):
                        self.log_string += '\n\t\t\t'+self.eval_measures[score][2]+' = '+str(scores[score])

                i += 1

            self.scores[id] = learner.scores
            self.stdev[id] = learner.stdev

    def paintscores(self):
        """paints the table with evaluation scores"""
        if self.logging:
            temp_string = '\t'.join([s[1] for s in self.eval_measures])
            self.log['results'] = 'Classifier\t' + temp_string

        if self.scores:
            self.resultsTable.setColumnCount(len(self.eval_measures)+1)


            header =  [s[1] for s in self.eval_measures]
            self.resultsTable.setHorizontalHeaderLabels(['Classifier']+header)


            prec_f = "%.3f"
            prec_g = "%.3g"

            ids = [l[0] for l in self.learners]
            learners = [l[1] for l in self.learners]

            self.resultsTable.setRowCount(len(self.learners))
            for (i, l) in enumerate(learners):
                self.resultsTable.setItem(i, 0, QTableWidgetItem(l.name))
                if self.logging:
                    # print in log file
                    self.log['results_'+str(i)] = l.name

            for (i, l) in enumerate(learners):
                temp = self.scores[ids[i]]
                scores = temp[str(self.currentTargetClass)]
                if scores:
                    if self.stdev:
                        temp2 = self.stdev[ids[i]]
                        stdev = temp2[str(self.currentTargetClass)]

                    for j in range(len(self.eval_measures)):
                        if self.stdevBtnStatus and stdev:
                            if stdev[j] > 10000.0 or stdev[j] < 0.00001:
                                prec2 = prec_g
                            else:
                                prec2 = prec_f
                            self.resultsTable.setItem(i, j+1, QTableWidgetItem( prec_f % scores[j]+" : "+prec2 % stdev[j]))
                            if self.logging:
                                # print into log file
                                self.log['results_'+str(i)] += '\t'+prec_f % scores[j]+" : "+prec2 % stdev[j]
                        else:
                            self.resultsTable.setItem(i, j+1, QTableWidgetItem(prec_f % scores[j]))
                            if self.logging:
                                # print into log file
                                self.log['results_'+str(i)] += '\t'+prec_f % scores[j]
                else:
                    for j in range(len(self.eval_measures)):
                        self.resultsTable.setItem(i, j+1, QTableWidgetItem(""))

            self.resultsTable.resizeColumnsToContents()

###########################################################################################################
###########################################################################################################

# Test the widget, run from DOS prompt

if __name__=="__main__":
    a = QApplication(sys.argv)
    ow = OWSubgroupEvaluation()
   ## a.setMainWidget(ow)

    filename1 = "..\\..\\doc\\datasets\\lenses.tab"
    filename2 = "..\\..\\doc\\datasets\\titanic.tab"
    if 'linux' in sys.platform:
        filename1= "/usr/doc/orange/datasets/lenses.tab"
        filename2= "/usr/doc/orange/datasets/titanic.tab"

    data1 = orange.ExampleTable(filename1)
    data2 = orange.ExampleTable(filename2)


    l1 = SD_learner(algorithm = 'SD', minSupport = 0.1, beamWidth = 5, g = 1)
    l1.name = 'SD'

    l2 = SD_learner(algorithm = 'Apriori-SD', minSupport = 0.2, minConfidence = 0.8)
    l2.name = 'Apriori - SD'

    l3 = SD_learner(algorithm = 'CN2-SD', k = 6)
    l3.name = 'CN2-SD'

    # if log print is needed
    ow.logging = True

    ow.setData(data1)
    ow.setLearner(l1,1)
    ow.setLearner(l2,2)
    ow.setLearner(l3,3)

    # the print of the log -----------------------------------------------------
    if ow.logging:
        reverse = []
        for e in range(len(ow.learners)): reverse = [e]+reverse
        for e in reverse:
            ow.log_string = '\n'+ow.log['results_'+str(e)]+ow.log_string
        ow.log_string = '\n'+ow.log['results']+ow.log_string

        ow.log_string = '\nSampling: Cross validation with number of folds '+str(ow.nFolds)+'\n'+ow.log_string
        ow.log_string = '\n-------------- EVALUATION OF SUBGROUP DISCOVERY ALGORITHMS -------------'+ow.log_string

        f = open('log_subgroup_evaluation.txt','w')
        f.write(ow.log_string)
        f.close()
        # --------------------------------------------------------------------------

    ow.show()
    a.exec_()

    ow.saveSettings()
