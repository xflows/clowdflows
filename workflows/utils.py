from workflows.models import *

def checkForCycles(widget,targetWidget):
    noCycles = True
    for c in Connection.objects.filter(input__widget=widget):
        if c.output.widget==targetWidget:
            noCycles = False
            return False
        noCycles = noCycles and checkForCycles(c.output.widget,targetWidget)
        if noCycles == False:
            return False
    return noCycles
