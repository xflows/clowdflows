Creating a widget - step by step
================================

About the example: REST Web service wrapper
-------------------------------------------

Here, we will wrap an existing piece of code into a ClowdFlows widget
step by step. The exemplary piece of code is a call to a REST Web
service, but it could be any other similar call. Namely, ClowdFlows
enables easy inclusion of WSDL Web services right in the user's
interface, but this does not work for the REST ones (yet).

We prepared a simple REST Web service for this example, which calculates
a sentiment score of a sentence. It is called like this:

http://kt.ijs.si/MartinZnidarsic/webservices/sentana/sentana.php?sentence=What+a+lovely+day

and returns a JSON response of such a kind:

::

    {"status":200,"status_message":"OK","data":{"num_positive_words":1,"num_negative_words":0,"sentimentscore":0.25,"lex_pos_length":2006,"lex_neg_length":4783}}

What we are interested in, is the sentimentscore value.

Since ClowdFlows is written in Python, we must wrap a call to this Web
service in a piece of Python code. A standalone piece of code for this
purpose could look like this:

.. code:: python

    import urllib2
    import json
    somesentence = "The good the bad and the ugly"
    somesentence = somesentence.replace (" ", "+")
    url = 'http://kt.ijs.si/MartinZnidarsic/webservices/sentana/sentana.php?sentence=' + somesentence
    response = urllib2.urlopen(url).read()
    jsondata = json.loads(response)
    print "Sentiment score is: " +  str( jsondata['data']['sentimentscore'] )

Creating the widget
-------------------

Step 1 : Enter the attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to create a widget, we should follow the instructions on
creating an abstract widget:
http://source.ijs.si/concrete/mothra/wikis/widget In our case, we did
the following on the web page where the widget information is to be
entered ( http://127.0.0.1:8000/admin/workflows/abstractwidget/add/ ):
\* in the Name category we entered ``Sentana`` \* as Action, we set
``call_sentana`` \* we have left the Wsdl and the Wsdl method empty \*
in Description we gave a short description of the widget and what kind
of inputs it expects \* for Category, we selected ``Creativity`` as we
want it to be shown under Creativity in the widget menu \* we left then
all the rest of the form empty down to the Package, where we set it to
the existing package ``creativity`` \* as a last step here, we defined
the inputs and outputs \*\* there was one input defined with \*\*\*
``text`` as its Name \*\*\* ``txt`` as its Short name \*\*\* and
``inp1`` as the Variable for that input \*\* and one output defined with
\*\*\* ``sentiment score`` as its Name \*\*\* ``ses`` as its Short name
\*\*\* and ``out1`` as the Variable for that output

We must press the Save button in order to save the information about the
widget. We can then leave the web page.

Step 2 : Implement the action
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Following the instructions, we create a function named ``call_sentana``
(because this name was entered as Action in Step 1). This function has
to be added in the library.py file of the widget's package. Since in our
case, the widget's package is ``creativity``, we have added it to
.../mothra/workflows/creativity/library.py

The action must receive the input\_dict and return an output\_dict,
which are both Python dictionaries. The simplest action function (just
copying the input to the output), as shown in the instructions, would
be:

.. code:: Python

    def my_package_action(input_dict):
        output_dict = {}
        output_dict['out1'] = input_dict['inp1']
        return output_dict

but we would like it to make the Sentana Web service call (similar as in
the example description), so we define it as:

.. code:: Python

    def call_sentana(input_dict):
        import urllib2
        import json
        somesentence = input_dict['inp1']  # our only input is in input_dict['inp1'] , notice the Variable name 'inp1'
        somesentence = somesentence.replace (" ", "+")
        url = 'http://kt.ijs.si/MartinZnidarsic/webservices/sentana/sentana.php?sentence=' + somesentence
        response = urllib2.urlopen(url).read()
        jsondata = json.loads(response)
        result = jsondata['data']['sentimentscore']
        output_dict = {}
        output_dict['out1'] = result  # result is put in the only output denoted with output_dict['out1']
        return output_dict

In such a function we could instead of a Web service call put a call to
something else just as easily.

Step 3 : Exporting your widget to the package data file
-------------------------------------------------------

When you have added or modified an abstract widget on that admin web
page, you made changes only to the local database. You must now also
export it to a package data file. In the instructions, the general
command given for this is

.. code:: bash

    python manage.py export_package $your_package_name$

in our case this is

.. code:: bash

    python manage.py export_package $your_package_name$

Step 4 : Check it out and make it available
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The widget should now appear in the widget menu of your locally served
ClowdFlows platform. Check it out, make a few test runs, use it and have
fun.

When you are ready to make your widget available to others, push it to
the ConCreTe ClowdFlows git repository: \* first make a fresh pull:
``git pull git@source.ijs.si:concrete/mothra.git`` \* if you receive
anything new, always run: ``python manage.py auto_import_packages``
(beware: this will run over your locally changed widgets, if you did not
export them yet as instructed in Step 3) \* add the changed files (the
library.py and any others) to be committed, e.g.:
``git add workflows/creativity/library.py`` \* commit, e.g.:
``git commit workflows/creativity/library.py`` \* push it from the local
repository to the ConCreTe one: ``git push``

That's it.
