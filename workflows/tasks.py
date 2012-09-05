from celery.task import task

@task()
def runForLoopIteration(workflow,iteration):
    pass


@task()
def runWidgetAsync(widget):
    widget.run(True)

@task()
def add(x, y):
    return x + y