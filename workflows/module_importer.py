from mothra.settings import INSTALLED_APPS

appName = 'workflows'

def get_installed_apps():
    return [name[len(appName)+1:] for name in INSTALLED_APPS if name.startswith(appName+'.') and len(name)>len(appName)+1]

#Following functions deal with imports of libraries as dicts
def import_all_packages_libs_as_dict(libName):
    pckLibs = {}
    for pck in get_installed_apps():
        pckLibs[pck] = import_package_lib_as_dict(pck, libName)
    return pckLibs

def import_package_lib_as_dict(packageName, libName):
    return dynamic_import_globals_as_dict(appName+"."+packageName + "." + libName, packageName)

def dynamic_import_globals_as_dict(name, package):
    try:
        m = __import__(name, globals(), locals(), ['*'])
    except:
        return None
    return m

#Following functions deal with imports of libraries as globals, hovever localSetAttrFunc must be provided - this function should set local global in file where we want import like:
#def setattr_local(name, value, package):
#    setattr(sys.modules[__name__], name, value)
def import_all_packages_libs(libName, localSetAttrFunc):
    for pck in get_installed_apps():
        import_package_lib(pck, libName, localSetAttrFunc)

def import_package_lib(packageName, libName, localSetAttrFunc):
    dynamic_import_globals(appName+"."+packageName + "." + libName, packageName, localSetAttrFunc)

def dynamic_import_globals(name, package, localSetAttrFunc):
    m = None
    try:
        m = __import__(name, globals(), locals(), ['*'])
    except:
        import sys, traceback
        print "Exception in user code:"
        print '-'*60
        traceback.print_exc(file=sys.stdout)
        print '-'*60
        return
    all_names = [name for name in dir(m) if name[0]!='_']
    g = globals()
    for name in all_names:
        #g[name] = m.__dict__.get(name)
        #print name
        localSetAttrFunc(name, m.__dict__.get(name), package)
