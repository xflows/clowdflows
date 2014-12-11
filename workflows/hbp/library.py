
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