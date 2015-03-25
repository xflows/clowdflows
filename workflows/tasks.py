from celery.task import task
import workflows.library

@task()
def add(a,b):
    import time
    time.sleep(10)
    return a+b

@task()
def runForLoopIteration(workflow,iteration):
    pass

@task()
def executeWidgetFunction(widget,input_dict):
    function_to_call = getattr(workflows.library,widget.abstract_widget.action)
    return function_to_call(input_dict)

@task()
def executeWidgetProgressBar(widget,input_dict):
    function_to_call = getattr(workflows.library,widget.abstract_widget.action)
    return function_to_call(input_dict,widget)

@task()
def executeWidgetStreaming(widget,input_dict):
    function_to_call = getattr(workflows.library,widget.abstract_widget.action)
    return function_to_call(input_dict,widget,None)

@task()
def executeWidgetWithRequest(widget,input_dict,output_dict,request):
    function_to_call = getattr(workflows.library,widget.abstract_widget.action)
    return function_to_call(request,input_dict,output_dict)

@task()
def executeWidgetPostInteract(widget,input_dict,output_dict,request):
    function_to_call = getattr(workflows.library,widget.abstract_widget.post_interact_action)
    return function_to_call(request,input_dict,output_dict)

@task()
def runWidget(widget,offline):
    widget.proper_run(offline)

@task()
def runWidgetAsync(widget):
    widget.run(True)

@task()
def runTest(return_string):
    import time
    time.sleep(3.2)
    return return_string
