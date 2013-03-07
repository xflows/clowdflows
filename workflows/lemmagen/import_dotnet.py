import logging
import os
import sys
from settings import PACKAGE_ROOT

#------------------------------------------------------------------------------
# prepare environment for loading latino (Python.net interpreter should be used)
# see: http://pythonnet.sourceforge.net/
#------------------------------------------------------------------------------

dllPath = os.path.join(PACKAGE_ROOT, 'bin')
sys.path.append(dllPath)

try:
    import System
    import LemmaSharp
    from LemmaSharpInterfaces import *

    from System.Reflection import Assembly
    Assembly.LoadFile(os.path.join(dllPath, 'LemmaSharpPrebuilt.dll'))
except Exception:
    logging.warning("DotNet assemblies could not be loaded! Probable reasons: missing dlls or wrong interpreter (see http://pythonnet.sourceforge.net). "
                    "Other functionality of ClowdFlows (besides .Net assemblies) should be OK!")
    pass

