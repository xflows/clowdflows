Installing requirements for the ILP and MySQL packages
======================================================

Dumping to Weka and Orange tables
---------------------------------

The mysql package requires the mysql-connector-python package. If you
installed everything in requirements.txt, you should already have it.
Otherwise:

.. code:: bash

    pip install mysql-connector-python

For dumping stuff to weka/orange data structures this is everything you
need.

Using MySQL databases with ILP widgets
--------------------------------------

To use local ILP widgets, you'll need yap prolog. To support 'smarter'
dumping of data, where the db connection is forwarded to yap, you'll
also need to compile yap with flags for myddas. Otherwise, you'll have
to use the 'dump full database' flags on the corresponding widgets
(Databse to RSD, Database to Aleph).

Yap with full database dumping
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Find the yap binaries for your OS or compile from source from `Yap
downloads <http://www.dcc.fc.up.pt/~vsc/Yap/downloads.html>`__. Use the
stable yap-6 version if possible.

Yap with mysql support
~~~~~~~~~~~~~~~~~~~~~~

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

