import logging
import sys
from settings import *

#------------------------------------------------------------------------------
# prepare environment for loading latino (Python.net interpreter should be used)
# see: http://pythonnet.sourceforge.net/
#------------------------------------------------------------------------------

sys.path.append(package_bin)

try:
    import System
    import Latino
    from LatinoCloudFlows import *
except Exception:
    logging.warning("DotNet assemblies could not be loaded! Probable reasons: missing dlls or wrong interpreter (see http://pythonnet.sourceforge.net). "
                    "Other functionality of ClowdFlows (besides .Net assemblies) should be OK!")
    pass

