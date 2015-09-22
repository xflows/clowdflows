Widget and Abstract Widget
==========================

There is a distinction between a Widget and an Abstract Widget In the
workflows app of the Mothra project.

Widgets are actual instances of widgets that belong to a specific
workflow and were created by a **user** of ClowdFlows. An abstract
widget is an object that is created by a ClowdFlows **developer** (or a
**user** in case of importing a Web Service). Abstract widgets do not
belong to specific workflows, but in categories.

When a user puts a widget on the canvas happens is this: a new widget is
created where all the information about the widget is copied from an
abstract widget. Inputs are created by copying abstract inputs (the same
with outputs). Almost every widget has a corresponding Abstract Widget
that it is derived from (the exceptions are the process control widgets
- the subprocess, input, output, for input, and for output).

When developing your own widgets, you will create Abstract Widgets.

Creating an Abstract Widget
===========================

To create an abstract widget navigate to:

http://127.0.0.1:8000/admin/workflows/abstractwidget/add/

Here you enter the attributes of the widget.

* **Name** is the name that will be displayed in the widget repository
  and under the actual widget itself.
* **Action** is the name of a python function that will be called when
  the widget is executed.
* **WSDL and WSDL method** are used if the widget is a call of a Web
  Service. Web Service widgets are usually not entered in the admin
  panel, but in the application itself by importing a Web Service.
* **Description** is used for a human readable description of what a
  widget does. A user will see this when he right clicks the widget and
  clicks help.
* **Category** determines to which category this widget belongs.
  Categories can be nested.
* **Visualization view** is (like the action) a python function that is
  a view that will render a template.
* If the **User** field is blank, everyone will see the widget,
  otherwise just this user. This is mainly used for Web Service imports
  as they are only visible to users that imported them.
* The widget can be **interactive**. This means that when a user
  executes the widget, the action will perform, then the **interaction
  view** will be executed and finally the **Post interact action** will
  be executed.
* **Image** and **Treeview image** are deprecated and will be phased
  out soon. Please use the **static image** field. This simplifies
  sending images to other installations of ClowdFlows. In the static
  image field just enter the filename of the image (without the path).
  The path will be :math:`package_name`/icons/widget/:math:`filename`
  and :math:`package_name`/icons/treeview/:math:`filename` where the
  treeview image is the small image that appears in the treeview on the
  left side and the widget image is the actual normal sized icon for
  the widget. **IMPORTANT**: the static image field only works if the
  **package** is set.
* The flag **has progress bar** determines if the widget implements a
  progress bar. Implementations of progress bars are displayed later on
  this wiki page.
* The **is streaming** flag is currently under construction, please do
  not use it yet.
* The **Order** determines the order in which the widget will be
  displayed in the repository. This is set automatically when sorting
  widgets in a single category from the admin.
* **UID** is set automatically when you export a package.
* **Package** is the package name. You are encouraged to use packages.

After you have filled in the attributes, enter the inputs and outputs.
Inputs have several more or less self explanatory attributes. You are
encouraged to set a **parameter type** for each input even if is not a
parameter, as users have the option to turn each input into a parameter
and vice-versa. If you choose the parameter type **select box** you have
to add Abstract Options to fill in the options. This can be done by
navigating to:

http://127.0.0.1:8000/admin/workflows/abstractinput/

Here you find your input and add the abstract options.

Another important attribute is the **multi** flag in the Abstract Input.
Inputs with this flag set will behave like this: whenever a connection
is added to this input another input will be created on the fly that
accepts the same data. In the action function, this will be represented
as a list.

The **variable** attribute of both the input and the output are
important because this is how the data will be accessed in the python
function that is executed when the widget runs.

Implementing the action of a widget
===================================

Let's say you have chosen **mypackage\_action** as the action (function
name), an input with the variable **inp1** and an output with the
variable **out1**. Open the **library.py** file in your package folder
and add the following:

::

    def my_package_action(input_dict):
        output_dict = {}
        output_dict['out1'] = input_dict['inp1']
        return output_dict

The action function takes a dictionary as an input and returns a
dictionary as an output. The keys of the dictionaries correspond to the
variables of inputs and outputs. **IMPORTANT**: if a widget has an
output with a variable that is not found in the dictionary when the
widget executes, an exception will be raised. So make sure you return a
dictionary with **all** the outputs.

This sample action function merely takes what is on the input and puts
it on the output.

Implementing the progress bar
=============================

If you wish to have a widget that takes a longer time to run and to
report its progress via a progress bar, you need to check the **has
progress bar** flag in the administration panel.

After that you need to do two thing:

* Modifiy your action function so that it takes a second argument (the
  instance of the widget itself)
* Update the progress bar in your action function when appropriate

Here is an example of an action of a widget that implements a progress
bar:

::

    def delay(input_dict,widget):
        widget.progress=0
        widget.save()
        timeleft = int(input_dict['time'])
        i = 0
        import time
        import math
        while i<timeleft:
            time.sleep(1)
            i=i+1
            widget.progress = math.floor(((i*1.0)/timeleft)*100)
            widget.save()
        widget.progress=100
        widget.save()
        output_dict = {}
        output_dict['data'] = input_dict['data']
        return output_dict

This is the delay widget that takes the number of seconds as an input
and sleeps for that amount of seconds. Each time after a sleep the
progress bar is updated. **IMPORTANT** the default runserver and
runserver\_plus will not show progress bars as they cannot handle
threads. To see progress bars in your local installation of ClowdFlows,
please use Django Devserver.

https://github.com/dcramer/django-devserver

The widget must be saved each time the progress bar is updated. The
widget.progress atribute must be an integer between 1 and 100. Only use
100 when the widget has finished executing, because the user interface
will stop polling for the progress when it is at 100.
