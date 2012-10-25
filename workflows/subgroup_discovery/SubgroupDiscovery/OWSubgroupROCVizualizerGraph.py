from OWGraph import *
import sys

class OWSubgroupROCVizualizerGraph(OWGraph):
    def __init__(self, parent = None, name =  "SubgroupROCVizualizerGraph"):
        "Constructs the graph"
        OWGraph.__init__(self, parent, name)
        #self.addMarker( "tocka", 100, 100, alignment = -1, bold = 0)
        self.clearGraph()     
        self.setShowXaxisTitle(1)
        self.setXaxisTitle("FP Rate (1-Specificity)")
        self.setShowYLaxisTitle(1)
        self.setYLaxisTitle("TP Rate (Sensitivity)")
        self.setMainTitle("Visualization of subgroups in the ROC space")        

    def clearGraph(self):
        self.clear()
        white = QColor(255,255,255)
        self.addCurve("dots", white, white, 1, style = QwtPlotCurve.NoCurve, symbol = QwtSymbol.Ellipse, xData =[1,0], yData = [1,0]) 
        self.setAxisScale(QwtPlot.xBottom, 0,1, 0.1)
        self.setAxisScale(QwtPlot.yLeft, 0, 1, 0.1)		

      

if __name__=="__main__":
    appl = QApplication(sys.argv)
    ow = OWSubgroupROCVizualizerGraph()
    #appl.setMainWidget(ow)
    ow.show()
    appl.exec_()

