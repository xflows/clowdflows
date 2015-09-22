Creating an external package
============================

It is possible to create external ClowdFlows packages which can be
installed with pip and included into ClowdFlows.

This allows us to have separate repositories for different packages. As
time will progress I will move most of the packages to external packages
so that we will keep only the bare minimum in the main ClowdFlows
repository.

Here is an example of an external package:

https://github.com/anzev/rdm

The Python package is called rdm and features two ClowdFlows packages:

::

    rdm.db
    rdm.wrappers

Each of these packages is pretty much the same as all the internal
ClowdFlows packages in the workflows folder.

In order to import these packages you must do the following:

-  add 'rdm.db' and 'rdm.wrappers' to your
   INSTALLED\_APPS\_EXTERNAL\_PACKAGES setting (an example is shown in
   \_\_local\_settings.py)

   -  Do one of the following:

      -  import each package separately

         -  python manage.py import\_package rdm.db
         -  python manage.py import\_package rdm.wrappers

      -  import all packages (which will also import external packages)

         -  python manage.py import\_all

Exporting the package after making changes to the database
----------------------------------------------------------

If I have an external package entitled 'extpackage' in the following
directory:

::

    /home/janez/extpackage/

and I wish to commit some changes that I have made to the abstract
widget database:

I export the package like this:

::

    python manage.py export_package extpackage /home/janez/extpackage/

this will export the package to /home/janez/extpackage/package\_data. I
can safely commit and push this to the repository of the external
package.
