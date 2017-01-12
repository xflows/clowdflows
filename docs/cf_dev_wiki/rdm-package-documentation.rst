RDM package
===========

The aim of the Relational Data Mining (RDM) package and tool is to make relational learning and inductive logic programming approaches publicly accessible.
The tool offers a common and easy-to-use interface to several relational learning algorithms and provides data access to several relational database management systems.

This RDM package ClowdFlows is an external package: it should be installed with pip and then included in ClowdFlows.

Prerequisites
-------------

* python >= 2.6
* mysql-connector-python (optionally, you can call the algorithms with their native input format)

Installing the Python-RDM package
---------------------------------

Latest release from PyPI::

    pip install python-rdm

Latest from GitHub::

    pip install https://github.com/anzev/rdm/archive/master.zip


Dumping to Weka and Orange tables
---------------------------------

The mysql-connector-python Python package is also required. If you
installed everything in requirements.txt, you should already have it.
Otherwise:

.. code:: bash

    pip install mysql-connector-python

For dumping stuff to weka/orange data structures this is everything you
need.


Yap with full database dumping
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Find the yap binaries for your OS or compile from source from `Yap
downloads <http://www.dcc.fc.up.pt/~vsc/Yap/downloads.html>`__. Use the
stable yap-6 version if possible.

Yap with mysql support
~~~~~~~~~~~~~~~~~~~~~~

To use local ILP widgets, you'll need yap prolog. To support 'smarter'
dumping of data, where the db connection is forwarded to yap, you'll
also need to compile yap with flags for myddas. Otherwise, you'll have
to use the 'dump full database' flags on the corresponding widgets
(Databse to RSD, Database to Aleph).

You'll need the mysql dev package. On debian systems:

.. code:: bash

    sudo apt-get install libmysqlclient-dev

Compiling yap:

.. code:: bash

    git clone git://yap.dcc.fc.up.pt/yap-6
    cd yap-6
    mkdir build
    cd build
    ../configure --enable-tabling --enable-myddas
    make

If everything went ok, run yap to see if myddas was successfully
compiled with yap. Run:

.. code:: bash

    ./yap

you should see something like:

.. code:: bash

    YAP 6.2.3 (x86_64-linux): Mon Apr  8 11:19:20 CEST 2013
    MYDDAS version MYDDAS-0.9.1
       ?-

Install:

.. code:: bash

    make install


Prerequisites of specific ILP/RDM algorithms
--------------------------------------------

Depending on what algorithms you wish to use, these are their dependencies.

Aleph and RSD
^^^^^^^^^^^^^

* yap prolog (preferably with ``--tabling`` enabled)

TreeLiker
^^^^^^^^^

* Java VM

Wordification
^^^^^^^^^^^^^

* orange 2.5 (this is planned to be dropped in favor of scikit-learn)
