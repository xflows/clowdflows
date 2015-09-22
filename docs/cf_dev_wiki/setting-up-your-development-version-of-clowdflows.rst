Local installation of the ClowdFlows project
============================================

This project used to be called mothra (internally) that's why there's
still some references to it in these instructions.

Prerequisites
-------------

-  python >= 2.5
-  pip
-  virtualenv/wrapper (optional)
-  python headers if you're compiling Pillow from source: you need the
   ``python-dev`` package on debian systems

Installation
------------

Creating the environment
~~~~~~~~~~~~~~~~~~~~~~~~

Create a virtual python environment for the project. If you're not using
virtualenv or virtualenvwrapper you may skip this step.

For virtualenvwrapper
`````````````````````

.. code:: bash

    mkvirtualenv --no-site-packages mothra-env

For virtualenv
``````````````

.. code:: bash

    virtualenv --no-site-packages mothra-env
    cd mothra-env
    source bin/activate

Clone the code
~~~~~~~~~~~~~~

Obtain the url to your git repository.

.. code:: bash

    git clone git@github.com:janezkranjc/clowdflows.git

Install requirements
~~~~~~~~~~~~~~~~~~~~

.. code:: bash

    cd clowdflows
    pip install -r requirements.txt

Configure project
~~~~~~~~~~~~~~~~~

.. code:: bash

    cp mothra/__local_settings.py mothra/local_settings.py
    vi mothra/local_settings.py

Enable workflow packages
~~~~~~~~~~~~~~~~~~~~~~~~

Uncomment the packages that you need in ``mothra/local_settings.py`` in
the ``INSTALLED_APPS_WORKFLOWS_SUB`` tuple.

Sync database
~~~~~~~~~~~~~

Say "no" to creating a super-user when prompted. You'll create the user
after migrations.

.. code:: bash

    python manage.py syncdb --noinput

Migrate database
~~~~~~~~~~~~~~~~

.. code:: bash

    python manage.py migrate

Create super-user
~~~~~~~~~~~~~~~~~

.. code:: bash

    python manage.py createsuperuser

Import packages
~~~~~~~~~~~~~~~

.. code:: bash

    python manage.py import_all

Running
-------

.. code:: bash

    python manage.py runserver

Running with debugger
---------------------

.. code:: bash

    python manage.py runserver_plus

Open browser to http://127.0.0.1:8000
