ClowdFlows packages
===================

ClowdFlows **packages** are used to transfer widget descriptions from
one installation of ClowdFlows to another. It is not necessary for a
widget to belong to a specific package. It is recommended that you
create widgets within packages if you wish to transfer these widgets to
the public installation of ClowdFlows or if you want someone else to use
your work without the headache of registering each widget in the
administration panel in a particular ClowdFlows installation.

Creating a package from the package template
--------------------------------------------

To start a new package from a template use the built-in management
command like this:

.. code:: bash

    python manage.py new_package_from_template $new_package_name$

Replace :math:`new_package_name` with a package name. **IMPORTANT:**
Please avoid naming packages after built-in Python or Django components.
In particular, this means you should avoid using names like django
(which will conflict with Django itself), orange, or test (which
conflicts with a built-in Python package).

This command creates a new package based on **package\_template**. The
package template is empty so you will need to add some new widgets and
export them so that you may import them elsewhere.

New package overview
--------------------

When you create a package called new\_package a new folder will appear
in the clowdflows/workflows folder with the same name as your package.

The following files and folders will be created:

::

    new_package/templates
    new_package/templates/interactions
    new_package/templates/visualizations
    new_package/library.py
    new_package/urls.py
    new_package/__init__.pyc
    new_package/visualization_views.py
    new_package/interaction_views.py
    new_package/views.py
    new_package/__init__.py
    new_package/settings.py
    new_package/library.pyc
    new_package/static
    new_package/static/new_package
    new_package/static/new_package/icons
    new_package/static/new_package/icons/widget
    new_package/static/new_package/icons/treeview

Important things you should know:

-  **static new\_package subfolder** - there is a subfolder in the
   static folder with the same name as the package. This is used for
   widget icons. If you add any other folders here it may clash with
   other static files, so you are advised to only put icons, css and
   other files inside the new\_package/static/new\_package folder.
-  **mothra/local\_settings.py** and
   \*\*mothra/\_\_local\_settings.py\*\* contain a tuple
   **INSTALLED\_APPS\_WORKFLOWS\_SUB** into which you should add an item
   ``'workflows.new_package'`` so that the code looks somewhat like
   this:

.. code:: python

    INSTALLED_APPS_WORKFLOWS_SUB = (
        'workflows.base',
        'workflows.new_package',
        ...

Exporting your custom widgets to the package data file
------------------------------------------------------

When you have added or modified an abstract widget you can export it so
that they may be included to the codebase.

.. code:: bash

    python manage.py export_package $your_package_name$

Package manager
===============

The package manager functions in such a way that each widget and each
category is saved into a separate json file, which makes it easier to
work with git and to manually change some attributes without having to
load up the server and doing it in the django admin.

There are four commands that are related to the package manager:

-  ``export_package``

   python manage.py export\_package base

This command exports all the widgets in the package base. It will also
export all categories, which contain at least one widget from the
package. The command will output which widgets were new, which were
changed and will alert you if there are no changes.

-  ``import_package``

   python manage.py import\_package base

This command will import all the exported widgets and categories from
the base package. Same as export, the command will output which widgets
and categories have actually changed (and which are new, if there are
any).

-  ``export_all``

   python manage.py export\_all

This command goes through all the INSTALLED\_APPS that start with
``workflows.`` and exports them the same as export\_package.

-  ``import_all``

   python manage.py import\_all

This command goes through all the INSTALLED\_APPS that start with
``workflows.`` and imports them.
