ClowdFlows RDM package documentation
=================================================

This is the documentation for the integration of the Python-RDM tool in the ClowdFlows data mining platform.
This widgets of this package can be found in the categories named ILP in ClowdFlows.

The aim of the Relational Data Mining (RDM) package and tool is to make relational learning and inductive logic programming approaches publicly accessible.
The tool offers a common and easy-to-use interface to several relational learning algorithms and provides data access to several relational database management systems.
We have also developed a stand-alone Python library https://github.com/xflows/rdm .



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
