from workflows.models import *

def checkForCycles(widget,targetWidget):
    noCycles = True
    for c in Connection.objects.filter(input__widget=widget):
        if Widget.objects.get(outputs__pk=c.output_id)==targetWidget:
            noCycles = False
            return False
        noCycles = noCycles and checkForCycles(Widget.objects.get(outputs__pk=c.output_id),targetWidget)
        if noCycles == False:
            return False
    return noCycles
