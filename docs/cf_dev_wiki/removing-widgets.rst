It is now possible to remove widgets from exported packages. You can do
so by physically moving an exported json file to a directory named
deprecated\_widgets which should be located in the package\_data
directory.

E.g. we want to remove the Create String widget from the base package:

::

    mkdir workflows/base/package_data/deprecated_widgets
    mv workflows/base/package_data/widgets/1b38bbab-7f89-4469-94cd-2f481f9c61f7.json workflows/base/package_data/deprecated_widgets/.

python manage.py import\_package base

The import package command will do the following for each deprecated
widget:

Check if there are any acutal widgets using this abstract widget \* if
so: the command prints out a warning in red colors to notify the
developer that the widget was not removed because it is used in
workflows. \* if not: the abstract widget gets deleted from the
database.

If you have deprecated widgets in your database and you export the
package the system will notify you that you are trying to export
deprecated widgets and they will not be exported. All other changes to
non-deprecated widgets will still be exported.
