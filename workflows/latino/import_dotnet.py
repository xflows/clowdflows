import logging
import os
import sys
from settings import PACKAGE_ROOT

#------------------------------------------------------------------------------
# prepare environment for loading latino (Python.net interpreter should be used)
# see: http://pythonnet.sourceforge.net/
#------------------------------------------------------------------------------

sys.path.append(os.path.join(PACKAGE_ROOT, 'bin'))

try:
    import System
    import Latino
    from LatinoClowdFlows import *
except Exception:
    logging.warning("DotNet assemblies could not be loaded! Probable reasons: missing dlls or wrong interpreter (see http://pythonnet.sourceforge.net). "
                    "Other functionality of ClowdFlows (besides .Net assemblies) should be OK!")
    pass

