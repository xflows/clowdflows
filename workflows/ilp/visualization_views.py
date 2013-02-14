from django.shortcuts import render
import json

def ilp_sdmsegs_rule_viewer(request,input_dict,output_dict,widget):
    d = json.loads(input_dict['sdmsegs_rules'])
    output = {}
    for k, v in d['A']['WRAcc'].items():
        terms = []
        for t in v['terms']:
            try:
                terms.append(d['ontDict'][t])
            except Exception, e:
                pass
        output[int(k)] = {
            'name': terms,
            'topGenes': int(len(v['topGenes'])),
            'allGenes': int(len(v['allGenes'])),
            'wracc': round(v['scores']['wracc'], 3)
        }
    return render(request, 'visualizations/sdmsegs_viewer.html',{'widget':widget,'input_dict':input_dict,'rules':output})