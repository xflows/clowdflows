import os

# === STANDARD PACKAGE SETTINGS ===
PACKAGE_ROOT = os.path.dirname(__file__)

# === AUTO IMPORT OPTIONS ===
#If auto_import_package_data is true then given data file is automatically imported when ClowdFlows project is newly deployed or refreshed from git
AUTO_IMPORT_DB = True
#For auto_import_package_data_replace_option description see the 'replace' option in workflows/import_package command
AUTO_IMPORT_DB_REPLACE_OPTION = True
#If file(s) other than ./db/package_data.json should be imported, auto_import_package_data_files should be corrected
AUTO_IMPORT_DB_FILES = [os.path.join(PACKAGE_ROOT,'db/package_data.json')]

