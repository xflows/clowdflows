from mothra.settings import INSTALLED_APPS

appName = 'workflows'
import workflows.library

def getAllInstalledAps():
    return [name[len(appName)+1:] for name in INSTALLED_APPS if name.startswith(appName+'.') and len(name)>len(appName)+1]

def importAllPackagesLib(libName, localSetattr):
    for pck in getAllInstalledAps():
        importPackageLib(pck, libName, localSetattr)

def importPackageLib(packageName, libName, localSetattr):
    dynamicImportAllGlobals(appName+"."+packageName + "." + libName, packageName, localSetattr)

def dynamicImportAllGlobals(name, package, localSetattr):
    try:
        m = __import__(name, globals(), locals(), ['*'])
    except:
        return
    all_names = [name for name in dir(m) if name[0]!='_']
    g = globals()
    for name in all_names:
        #g[name] = m.__dict__.get(name)
        #print name
        localSetattr(name, m.__dict__.get(name), package)

