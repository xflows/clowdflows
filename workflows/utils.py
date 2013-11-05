from workflows.models import *
from workflows.toposort import toposort

def checkForCycles(widget,targetWidget):
    pairs = []
    for c in widget.workflow.connections.select_related("output","input").defer("output__value","input__value").all():
        if not (c.output.widget_id,c.input.widget_id) in pairs:
            pairs.append((c.output.widget_id,c.input.widget_id))
    try:
        toposort(pairs,None,True,True)
    except:
        return False
    return True

def slowCheckForCycles(widget,targetWidget):
    noCycles = True
    for c in Connection.objects.filter(input__widget=widget):
        if Widget.objects.get(outputs__pk=c.output_id)==targetWidget:
            noCycles = False
            return False
        noCycles = noCycles and checkForCycles(Widget.objects.get(outputs__pk=c.output_id),targetWidget)
        if noCycles == False:
            return False
    return noCycles
