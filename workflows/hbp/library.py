
def hbp_interactive_analysis(input_dict):
    return {}

def hbp_interactive_analysis_post(postdata,input_dict,output_dict):
    import orange
    import tempfile
    import arff
    import os
    import numpy

    d = input_dict['Dataset']

    dset = arff.load(open(os.path.dirname(os.path.abspath(__file__))+"/new_adni.arff",'rb'))

    b = {}
    b['attributes'] = []

    feature_names = [x.name for x in d.domain]

    if 'Classification' in feature_names:
        b['attributes'].append(('Classification',[u'CN', u'AD', u'LMCI', u'EMCI', u'SMC']))

    if 'geo' in feature_names:
        b['attributes'].append(('geo',[u'Africa', u'Asia', u'Australia', u'Europe', u'North America', u'South America']))

    if 'age' in feature_names:
        b['attributes'].append(('age',[u'1-18', u'18-24', u'25-34', u'35-44', u'45-54', u'55-64', u'65+']))            

    for v in postdata['variables']:
        b['attributes'].append((v+"_avg","NUMERIC"))
        b['attributes'].append((v+"_stdev","NUMERIC"))

    b['attributes'].append(('count','NUMERIC'))

    b['description'] = ""
    b['relation'] = "HBP"
    b['data'] = []

    for i in d:
        new_i = []
        if 'Classification' in feature_names:
            clas = i[feature_names.index('Classification')].value
            new_i.append(clas)
        else:
            clas = None

        if 'geo' in feature_names:
            geo = i[feature_names.index('geo')].value
            new_i.append(geo)
        else:
            geo = None

        if 'age' in feature_names:
            age = i[feature_names.index('age')].value
            new_i.append(age)
        else:
            age = None


        sums = {}
        for v in postdata['variables']:
            sums[v] = 0

        vcounts = {}
        for v in postdata['variables']:
            vcounts[v] = 0


        vvalues = {}
        for v in postdata['variables']:
            vvalues[v] = []

        count = 0

        attrs = [x[0] for x in dset['attributes']]

        for ins in dset['data']:
            if (ins[-1]==clas or clas==None) and (ins[0]==geo or geo==None) and (ins[1]==age or age==None):
                count = count + 1
                for k,v in sums.items():
                    try:
                        sums[k] = v + ins[attrs.index(k)]
                        vcounts[k] = vcounts[k]+1
                        vvalues[k].append(v)
                    except:
                        pass

        for v in postdata['variables']:
            print vvalues[v]
            stdev = numpy.std(vvalues[v])
            try:
                avg = sums[v]/vcounts[v]
            except:
                avg = -2
            new_i.append(avg)
            new_i.append(stdev)
        new_i.append(count)
        b['data'].append(new_i)

    f = tempfile.NamedTemporaryFile(delete=False,suffix='.arff')
    f.write(arff.dumps(b))
    f.close()
    output_dict = {}
    output_dict['results'] = orange.ExampleTable(f.name)
    return output_dict

def hbp_search_criteria(input_dict):
    return {}

def hbp_search_criteria_post(postdata,input_dict,output_dict):
    query = {}
    query['classification'] = postdata.get('d',None)
    query['geo'] = postdata.get('g',None)
    query['age'] = postdata.get('a',None)
    return {'query':query}

def hbp_submit_search_criteria(input_dict):
    import orange
    import tempfile
    import arff
    import os
    
    dset = arff.load(open(os.path.dirname(os.path.abspath(__file__))+"/new_adni.arff",'rb'))

    b = {}
    query = input_dict['query']

    b['attributes'] = []

    if query['classification'] != None:
        b['attributes'].append(('Classification',[u'CN', u'AD', u'LMCI', u'EMCI', u'SMC']))
    else:
        query['classification']=[None,]

    if query['geo'] != None:
        b['attributes'].append(('geo',[u'Africa', u'Asia', u'Australia', u'Europe', u'North America', u'South America']))
    else:
        query['geo'] =[None,]

    if query['age'] != None:
        b['attributes'].append(('age',[u'1-18', u'18-24', u'25-34', u'35-44', u'45-54', u'55-64', u'65+']))
    else:
        query['age'] = [None,]

    fields = []
    for c in query['classification']:
        for g in query['geo']:
            for a in query['age']:
                fields.append((c,g,a))

    b['attributes'].append(('count','NUMERIC'))


    b['description'] = ""
    b['relation'] = "HBP"
    b['data'] = []

    for field in fields:
        d = []
        if field[0] != None:
            d.append(field[0])
        if field[1] != None:
            d.append(field[1])
        if field[2] != None:
            d.append(field[2])
        counts = 0
        for i in dset['data']:
            if (i[-1]==field[0] or field[0]==None) and (i[0]==field[2] or field[2]==None) and (i[1]==field[1] or field[1]==None):
                counts = counts+1

        d.append(counts)
        
        b['data'].append(d)

    #a = arff.load_data()
    f = tempfile.NamedTemporaryFile(delete=False,suffix='.arff')
    f.write(arff.dumps(b))
    f.close()
    output_dict = {}
    output_dict['results'] = orange.ExampleTable(f.name)
    return output_dict