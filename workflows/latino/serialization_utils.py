from import_dotnet import *
import time
import logging
logging.basicConfig(format='%(asctime)s %(message)s', level=logging.INFO)


#------------------------------supposed----------------------------------------
# Generic interfaces for pickling .net or standard objects
#------------------------------------------------------------------------------

#check if .net object and wrap it accordingly
def GetBaseOrLSO(obj):
    #print "----------------------------------------------------------------------------------"
    #print hasattr(obj, "GetType")
    if (hasattr(obj, "GetType")):
        #print System.Object().GetType().IsAssignableFrom(obj.GetType())
        #print "----------------------------------------------------------------------------------"
        if System.Object().GetType().IsAssignableFrom(obj.GetType()):
            try:
                srlz = Latino.ISerializable(obj)
                return LSO(srlz)
            except:
                return obj
        else:
            return obj
    else:
        #print "----------------------------------------------------------------------------------"
        return obj

#------------------------------supposed------------------------------------------------
# Generic interfaces for pickling .net objects
#------------------------------------------------------------------------------
class SerializableObject:
    netObj = None
    constructor = '<constructor_func_name>'
    def __init__(self, object = None):
        self.netObj = object
        self.copyAttributes()
    def createInstance(self, dict):
        constr = eval(self.constructor)
        args = self.getConstructorArgs(dict)
        self.netObj = constr(*args)
        self.copyAttributes()
    def getConstructorArgs(self, dict):
        if dict.has_key('constructorArgs'):
            return tuple([val for key,val in dict['constructorArgs'].items()])
        return ()
    def copyAttributes(self): #copy all attributes from wrapped object to the wrapper
        pass
        #for attr in dir(self.netObj):
        #    if hasattr(self.netObj, attr) and not hasattr(self, attr):
        #        setattr(self, attr, getattr(self.netObj, attr))
    def __repr__(self):
        #return "<" + self.netObj.__str__() + " wrapped in "+self.__class__.__name__+">"
        return "<" + self.netObj.__str__() + ">"
    def netObj(self):
        return self.netObj

class LatinoSerializableObject(SerializableObject):
    def __getstate__(self):
        logging.info('Serialize {0} with latino serializer (start)'.format(self.netObj.__class__.__name__))
        start = time.clock()
        byteData = LatinoCF.Save(self.netObj)
        elapsed = (time.clock() - start)
        logging.info('Serialize {0} with latino serializer (end, size: {1:,} chars) in {2} ms'.format(self.netObj.__class__.__name__, len(byteData),elapsed))
        return {'byteData': byteData}
    def __setstate__(self, dict):
        logging.info('Deserialize {0} with latino deserializer (start)'.format("<LatinoObject>"))
        start = time.clock()
        self.netObj = LatinoCF.Load(dict['byteData'])
        self.copyAttributes()
        elapsed = (time.clock() - start)
        logging.info('Deserialize {0} with latino deserializer (end, size: {1:,} chars) in {2} ms'.format(self.netObj.__class__.__name__, len(dict['byteData']),elapsed))
    def __repr__(self):
        return "<LSO: " + self.netObj.__str__() + ">"

LSO = LatinoSerializableObject

class ObjectFunctionSerializableObject(SerializableObject):
    save = '<save_data_func_name>'
    load = '<load_data_func_name>'
    def __getstate__(self):
        logging.info('Serialize {0} with obj.function {1} (start)'.format(self.__class__.__name__, self.save))
        saveFunc = getattr(self.netObj, self.save)
        data = saveFunc()
        logging.info('Serialize {0} with obj.function {1} (end, size: {2:,} chars)'.format(self.__class__.__name__, self.save, len(data)))
        return {'data': data}
    def __setstate__(self, dict):
        logging.info('Deserialize {0} with obj.function {1} (start)'.format(self.__class__.__name__, self.load))
        self.createInstance(dict)
        loadFunc = getattr(self.netObj, self.load)
        loadFunc(dict['data'])
        logging.info('Deserialize {0} with obj.function {1} (end, size: {2:,} chars)'.format(self.__class__.__name__, self.load, len(dict['data'])))

class ExternalFunctionSerializableObject(SerializableObject):
    save = '<save_data_func_name>'
    load = '<load_data_func_name>'
    def __getstate__(self):
        logging.info('Serialize {0} with ext.function {1} (start)'.format(self.__class__.__name__, self.save))
        saveFunc = eval(self.save)
        data = saveFunc(self.netObj)
        logging.info('Serialize {0} with ext.function {1} (end, size: {2:,} chars)'.format(self.__class__.__name__, self.save, len(data)))
        return {'data': data}
    def __setstate__(self, dict):
        logging.info('Deserialize {0} with ext.function {1} (start)'.format(self.__class__.__name__, self.load))
        loadFunc = eval(self.load)
        self.netObj = loadFunc(dict['data'])
        self.copyAttributes()
        logging.info('Deserialize {0} with ext.function {1} (end, size: {2:,} chars)'.format(self.__class__.__name__, self.load, len(dict['data'])))

class PropertySerializableObject(SerializableObject):
    constructorArgs = []
    props = []
    def __getstate__(self):
        logging.info('Serialize {0} properties (start)'.format(self.__class__.__name__))
        dictToSave = {'props':{},'constructorArgs':{}}
        for prop in self.props:
            if hasattr(self.netObj, prop):
                dictToSave['props'][prop] = getattr(self.netObj, prop)
        for constructorArg in self.constructorArgs:
            if hasattr(self.netObj, constructorArg):
                dictToSave['constructorArgs'][constructorArg] = getattr(self.netObj, constructorArg)
        logging.info('Serialize {0} properties (end {1:,} properties)'.format(self.__class__.__name__, len(dictToSave['props'])+len(dictToSave['constructorArgs'])))
        return dictToSave
    def __setstate__(self, dict):
        logging.info('Deserialize {0} properties (start)'.format(self.__class__.__name__))
        self.createInstance(dict)
        for prop in dict:
            if hasattr(self.netObj, prop):
                setattr(self.netObj, prop, dict[prop])
        logging.info('Deserialize {0} properties (end {1:,} properties)'.format(self.__class__.__name__, len(dict['props'])+len(dict['constructorArgs'])))

class ManuallySerializableObject(SerializableObject):
    def __getstate__(self):
        logging.info('Serialize {0} manually (start)'.format(self.__class__.__name__))
        dictToSave = self.getstate()
        logging.info('Serialize {0} manually ({1:,} properties)'.format(self.__class__.__name__, len(dictToSave)))
        return dictToSave
    def __setstate__(self, dict):
        logging.info('Deserialize {0} manually start)'.format(self.__class__.__name__))
        self.createInstance(dict)
        self.setstate(dict)
        logging.info('Deserialize {0} manually ({1:,} properties)'.format(self.__class__.__name__, len(dict)))
    def getstate(self):
        return {}
    def setstate(self, dict):
        pass

#------------------------------------------------------------------------------
# Helper functions
#------------------------------------------------------------------------------
def ToInt(s):
    try:
        return int(s)
    except:
        return 0
def ToFloat(s):
    try:
        return float(s)
    except:
        return 0
def ToBool(s):
    if s == 'true' or s == 'True':
        return True
    else:
        return False
def ToString(s):
    return s
def ToEnum(typ, s,defaultValue):
    return LatinoCF.ParseEnum[typ](s, defaultValue)
def ToIntList(s):
    il = []
    for i in s:
        il.append(ToInt(i))
    return il
def ToPyList(l):
    return [x for x in l]
def ToNetList(Type,l):
    a = System.Array[Type](l)
    return a
def IsSequence(arg):
    return (not hasattr(arg, "strip") and
            hasattr(arg, "__getitem__") or
            hasattr(arg, "__iter__"))
def Flatten(l, ltypes=(list, tuple)):
    ltype = type(l)
    l = list(l)
    i = 0
    while i < len(l):
        while isinstance(l[i], ltypes):
            if not l[i]:
                l.pop(i)
                i -= 1
                break
            else:
                l[i:i + 1] = l[i]
        i += 1
    return ltype(l)

def ToNetObj(data):
    if hasattr(data, "netObj"):
        return data.netObj
    if isinstance(data, dict):
        if not len(data):
            return System.Collections.Generic.Dictionary[System.Object,System.Object]()
        else:
            key = ToNetObj(data.keys()[0])
            val = ToNetObj(data.values()[0])
            for tryIndex in [0, 1]:
                try:
                    if not tryIndex:
                        d = System.Collections.Generic.Dictionary[type(key),type(val)]()
                    else:
                        d = System.Collections.Generic.Dictionary[System.Object,System.Object]()
                    for key,val in data.items():
                        k = ToNetObj(key)
                        v = ToNetObj(val)
                        d[k] = v
                    return d
                except:
                    pass
            return System.Object()
    if isinstance(data, list):
        if not len(data):
            return System.Collections.Generic.List[System.Object]()
        else:
            dataNet = ToNetObj(data[0])
            for tryIndex in [0, 1]:
                try:
                    if not tryIndex:
                        l = System.Collections.Generic.List[type(dataNet)]()
                    else:
                        l = System.Collections.Generic.List[System.Object]()
                    for i, val in enumerate(data):
                        l.Add(ToNetObj(val))
                    return l
                except:
                    pass
            return System.Object()
    if isinstance(data, tuple):
        if not len(data):
            return System.Collections.Generic.LinkedList[System.Object]()
        else:
            dataNet = ToNetObj(data[0])
            for tryIndex in [0, 1]:
                try:
                    if not tryIndex:
                        a = System.Collections.Generic.LinkedList[type(dataNet)]()
                    else:
                        a = System.Collections.Generic.LinkedList[System.Object]()
                    for i, val in enumerate(data):
                        a.AddLast(ToNetObj(val))
                    return a
                except:
                    pass
            return System.Object()
    return data
def ToPyObj(data):
    if hasattr(data, "GetType") and data.GetType().IsGenericType:
        genType = data.GetType().GetGenericTypeDefinition()
        if genType.Equals(System.Collections.Generic.Dictionary):
            d = {}
            for keyVal in data:
                k = ToPyObj(keyVal.Key)
                v = ToPyObj(keyVal.Value)
                d[k] = v
            return d
        if genType.Equals(System.Collections.Generic.List):
            l = []
            for val in data:
                l.append(ToPyObj(val))
            return l
        if genType.Equals(System.Collections.Generic.LinkedList):
            l = []
            for val in data:
                l.append(ToPyObj(val))
            return tuple(l)
    if hasattr(data, "GetType"):
        for interface in data.GetType().GetInterfaces():
            if interface.Name == u'ISerializable':
                return LSO(data)
    return data
