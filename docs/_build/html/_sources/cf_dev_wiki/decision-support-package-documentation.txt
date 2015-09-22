ClowdFlows decision support package documentation
=================================================

This document provides a short overview of the decision support package.
The first section describes the organization, while the second section
describes the individual widgets and how they can be used.

Organization
------------

The package follows the recommended code structure of ClowdFlows
packages:

::

    workflows/decision_support/
    ├── db
    │   └── package_data.json
    ├── __init__.py
    ├── interaction_views.py
    ├── library.py
    ├── settings.py
    ├── static
    │   └── decision_support
    │       └── icons
    │           ├── treeview
    │           │   ├── piechart.png
    │           │   ├── sensitivity.png
    │           │   └── wsm.png
    │           └── widget
    │               ├── piechart.png
    │               ├── sensitivity.png
    │               └── wsm.png
    ├── templates
    │   ├── interactions
    │   │   └── wsm.html
    │   └── visualizations
    │       ├── ds_charts.html
    │       └── sensitivity_analysis.html
    ├── visualization_views.py
    └── wsm.py

The ``db`` folder contains the exported package data (widgets, inputs,
outputs and categories) in json. If changes are made to local widgets or
new widgets are created, the package data must be updated using the
``export_package`` management command.

The ``settings.py`` file contains package level settings, same as for
any ClowdFlows package, e.g., path to the package data file.

The ``static`` folder contains static files used by the package.
Currently these include only the icons.

The ``templates`` folder contains Django template HTML files. These are
divided into ``interactions`` and ``visualizations``.

Most widgets in ClowdFlows can be divided into three categories:
*regular*, *interactive* and *visualization* widgets. Regular widgets
simply take an input and produce an output, while interactive widgets
take an input, request some additional input from the user (e.g.,
setting weights of attributes) and produce an output. Visualization
widgets take an input and output a certain visualization based on the
input (e.g., displaying a chart or a dataset).

The widget views, i.e., functions which are called when a widget is
executed by the user, are divided into:

* ``library.py``
* ``interaction_views.py``
* ``visualization_views.py``

which reflect the categories listed above.

Of course the code can be divided into other Python modules. For
example, the ``wsm.py`` module includes the class implementing the
*Weighted sum model*, which is used in the views.

Interaction and visualization widgets have corresponding templates.
These (usually) also contain JavaScript for handling their GUIs.

Widgets
-------

The package includes the following widgets:

* Weighted sum model*, implementing a simple decision support model,
* Sensitivity analysis*, offering the mechanism to see how each alternative's score changes while changing the importance of one attribute,
* Decision support charts*, implementing several charts, which are useful for making reports and overviewing the data.

An example workflow can be found at:
http://clowdflows.org/workflow/383/.

Weighted sum model
~~~~~~~~~~~~~~~~~~

**Inputs:**

* ``odt``: Orange data table, with an optional ``label`` meta attribute

**Outputs:**

* ``odt``: Orange data table, with an added ``score`` column and with normalized attribute values
* ``mdl``: the WSM model object, which can be pickled and saved or used in the next two widgets.

**Functionality:**

When the widget is executed a popup is displayed, where the user can
assign a *weight* to each attribute, as well as the range of values of
the attribute and if the attribute should be *maximized* or *minimized*.

Weights can be *normalized* (so that they sum up to 100%) by clicking
the ``Normalize weights`` button and *reset* by clicking the ``Reset``
button. After clicking the ``Apply`` button, the new table and the model
are produced.

Sensitivity analysis
~~~~~~~~~~~~~~~~~~~~

**Inputs:**

*  ``mdl``: A decision support model, e.g.: a WSM object.

**Outputs:**

None.

**Functionality:**

When the widget is executed a popup is displayed, where the user can
choose which attribute to vary. After the selection, a graph
corresponding to the sensitivity analysis of this attribute is shown.
Thanks to the `Highcharts library <http://www.highcharts.com/>`__, the
graph can be modified by deselecting certain alternatives and it can be
saved in various formats (like PNG, PDF, SVG).

Decision support charts
~~~~~~~~~~~~~~~~~~~~~~~

**Inputs:**

* ``mdl``: A decision support model, e.g.: a WSM object.

**Outputs:**

None.

**Functionality:**

When the widget is executed a popup is displayed, where the user can
choose between four charts:

* Weights pie chart*: The assigned weights visualized as a pie chart.
* Weights bar chart*: The assigned weights visualized as a bar chart.
* Alternatives column chart*: The scores of the alternatives visualized as a column chart.
* Attribute values chart*: The values for each attribute for each alternative visualized as a bar chart.

Each chart can be saved in various formats (like PNG, PDF, SVG).
