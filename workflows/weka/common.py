__author__ = 'vid'

from base64 import b64encode, b64decode

import jpype as jp

from temputils import TemporaryFile
from os.path import join, normpath, dirname

BASE = normpath(dirname(__file__))
JARDIR = normpath(join(BASE, 'weka'))
CLASSPATH = '-Djava.ext.dirs=' + JARDIR  # + '-Djava.class.path=' + JARDIR
if not jp.isJVMStarted():
    jp.startJVM(jp.getDefaultJVMPath(), CLASSPATH)


def parse_options(op_string):
    return op_string.replace(',', ' ').split() if op_string != None else []


def serialize_weka_object(obj):
    s = jp.JClass('weka.core.SerializationHelper')
    tfile = TemporaryFile(flags='wb+')
    s.write(tfile.name, obj)
    return b64encode(tfile.fp.read())


def deserialize_weka_object(objString):
    d = jp.JClass('weka.core.SerializationHelper')
    tfile = TemporaryFile(flags='wb+')
    tfile.writeString(b64decode(objString))
    return d.read(tfile.name)
