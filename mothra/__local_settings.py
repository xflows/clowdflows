# Local settings for mothra project.
LOCAL_SETTINGS = True
from settings import *

DEBUG = True

DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.sqlite3', # Add 'postgresql_psycopg2', 'mysql', 'sqlite3' or 'oracle'.
        'NAME': os.path.join(PROJECT_DIR, 'mothra.db'), # Or path to database file if using sqlite3.
        'USER': '',                             # Not used with sqlite3.
        'PASSWORD': '',                         # Not used with sqlite3.
        'HOST': '',                             # Set to empty string for localhost. Not used with sqlite3.
        'PORT': '',                             # Set to empty string for default. Not used with sqlite3.
    }
}

USE_CONCURRENCY = False

FILES_FOLDER = os.path.join(PUBLIC_DIR, 'files/')

INSTALLED_APPS_WORKFLOWS_SUB = (
    'workflows.base',
    #'workflows.latino',
    #'workflows.decision_support',
    #'workflows.segmine',
    #'workflows.subgroup_discovery',
    #'workflows.nlp',
    #'workflows.nl_toolkit',
    #'workflows.ilp',
    #'workflows.weka',
    #'workflows.cforange',
    #'workflows.perfeval',
    #'workflows.mysql',
    #'workflows.lemmagen',
    #'workflows.crossbee',
    #'workflows.scikitAlgorithms',
    #'workflows.lemmagen',
    #'workflows.crossbee',
    #'workflows.streaming',
    #'workflows.bio3graph',
    #'workflows.noise',
    #'workflows.vipercharts',
    #'workflows.MUSE',
    #'workflows.hbp',
)

INSTALLED_APPS_EXTERNAL_PACKAGES = (
    #'rdm.db',
    #'rdm.wrappers'
)

BROKER_URL = 'django://'

# Make this unique, and don't share it with anybody.
SECRET_KEY = '*f$)twxl*rdk*o@^j%^0f0r#z7=kkyw=-2v*rjdnon_j==1uw@'

if DEBUG:
    # Show emails in the console during developement.
    EMAIL_BACKEND = 'django.core.mail.backends.console.EmailBackend'

CACHES = {}

import sys
reload(sys)
sys.setdefaultencoding('utf8')

USE_WINDOWS_QUEUE = False
