import os

# === STANDARD PACKAGE SETTINGS ===
package_root = os.path.dirname(__file__)

# === AUTO IMPORT OPTIONS ===
#If auto_import_package_data is true then package_data.json file is automatically imported when ClowdFlows project is newly deployed or refreshed from git
auto_import_package_data = True
#For auto_import_package_data_replace_option description see the 'replace' option in workflows/import_package command
auto_import_package_data_replace_option = True
#If file(s) other than ./db/package_data.json should be imported, auto_import_package_data_files should be corrected
auto_import_package_data_files = [os.path.join(package_root,'db/package_data.json')]

