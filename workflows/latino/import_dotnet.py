import logging
import sys
from settings import *

#------------------------------------------------------------------------------
# prepare environment for loading latino (Python.net interpreter should be used)
# see: http://pythonnet.sourceforge.net/
#------------------------------------------------------------------------------

sys.path.append(package_bin)

try:
    from LatinoCloudFlows import *
    import System
    import Latino
except Exception:
    logging.warning("LatinoClowdFlows could not be imported! Either there are no Latino dll available or a "\
                    "wrong interpreter is used. See 'http://pythonnet.sourceforge.net' for interpreter details. "\
                    "Other functionality (besides latino) will work as .")
    pass

logging.basicConfig(format='%(asctime)s %(message)s', level=logging.INFO)