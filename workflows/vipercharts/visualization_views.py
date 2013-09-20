import sys
from django.shortcuts import render
from django.http import Http404, HttpResponse

from workflows import module_importer

from django.shortcuts import render

def vipercharts_display_summation(request,input_dict,output_dict,widget):
    if sum(input_dict['intList']) == input_dict['sum']:
        check = 'The calculation appears correct.'
    else:
        check = 'The calculation appears incorrect!'
    return render(request, 'visualizations/vipercharts_display_integers.html',{'widget':widget,'input_dict':input_dict, 'output_dict':output_dict, 'check':check})

# Scatter charts
def vipercharts_pr_space_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/pr_space.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
	
def vipercharts_roc_space_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/roc_space.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})

# Curve charts	
def vipercharts_roc_curve_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/roc_curve.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
	
def vipercharts_roc_hull_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/roc_hull.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
	
def vipercharts_pr_curve_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/pr_curve.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
	
def vipercharts_lift_curve_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/lift_curve.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
	
def vipercharts_cost_curve_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/cost_curve.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
	
def vipercharts_kendall_curve_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/kendall_curve.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
	
def vipercharts_rate_curve_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/rate_curve.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})

# Column charts
def vipercharts_column_chart_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/column_chart.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
	
def vipercharts_eval_bar_chart_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/eval_bar_chart.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})
 
# Related results table 
def vipercharts_related_table_view(request,input_dict,output_dict,widget):
    return render(request, 'visualizations/related_results_table.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})