from django.shortcuts import render
import json

def ilp_sdmsegs_rule_viewer(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/sdmsegs_viewer.html',{'widget':widget,'rules':json.loads(input_dict['sdmsegs_rules'])})