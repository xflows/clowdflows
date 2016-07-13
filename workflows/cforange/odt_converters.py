def toKDICheader(table, try_numericize = 0):
    import cStringIO, string    
    t = table
    f = cStringIO.StringIO()

    f.write('\nDictionary\t%s\n'%t.name)
    f.write('{\n')

    ats = [i for i in t.domain.attributes]
    ats.append(t.domain.classVar)
    
    for i in ats:
        real = True
        if i.varType == 1:
            if try_numericize:
                # try if all values numeric
                for j in i.values:
                    try:
                        x = string.atof(j)
                    except:
                        real = False # failed
                        break
            else:
                real = False
        iname = str(i.name)
        if string.find(iname," ") != -1:
            iname = "'%s'"%iname
        if real:
            f.write('\tNumerical\t%s\t\t;\n'%iname)
        else:
            f.write('\tCategorical\t%s\t\t;\n'%iname)
    f.write('};\n')
        
    return f

def toKDICstring(table):
    import cStringIO, string    
    t = table
    f = cStringIO.StringIO()

    ats = [i for i in t.domain.attributes]
    ats.append(t.domain.classVar)
    # recuperate the attributes'names
    for i,e in enumerate(ats):
        iname = str(e.name)
        if i<len(ats)-1 :
            f.write('%s\t'%iname)
        else:  
        # remove the tabulation after the last item 
            f.write('%s'%iname) 
    f.write('\n')
        
    for j in t:
        x = []
        for i in range(len(ats)):
            s = str(j[i])
            if string.find(s," ") == -1:
                x.append("%s"%s)
            else:
                x.append("'%s'"%s)
        for i in x[:-1]:
            f.write('%s\t'%i)
        f.write('%s\n'%x[-1])
        
    return f

def toPRDstring(table):
    import cStringIO, string    
    t = table
    f = cStringIO.StringIO()

    f.write('--INDIVIDUAL\n')
    f.write('%s 1 %s cwa\n' % (t.name,t.name))
    f.write('--STRUCTURAL\n')
    f.write('--PROPERTIES\n')       
    f.write('class 2 %s #class cwa\n' % t.name)
    ats = [i for i in t.domain.attributes]
    ats.append(t.domain.classVar)
    for i in ats:
        iname = str(i.name)
        if string.find(iname," ") != -1:
            iname = "'%s'"%iname
        f.write('%s 2 %s #%s 1 cwa\n'%(iname,t.name,iname))

    return f

def toFCTstring(table):
    import cStringIO, string    
    t = table
    f = cStringIO.StringIO()

    ats = [i for i in t.domain.attributes]
    ats.append(t.domain.classVar)       
    for j in t:
        f.write('!\n')      
        x = []
        for i in range(len(ats)):
            s = str(j[i])
            if ats[i].name == t.domain.classVar.name:
                x.insert(0,"%s(%s,%s)"%(t.domain.classVar.name,j.id,s))
            elif string.find(s," ") == -1:
                x.append("%s(%s,%s)"%(ats[i].name,j.id,s))
            else:
                x.append("'%s'"%s)
        for i in x:
            f.write('%s\n'%(i))
        
    f.write('\n')
    return f
