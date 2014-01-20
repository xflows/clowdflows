from django.shortcuts import render

def base_wordcloud(request,input_dict,output_dict,widget,iframe=False):
    if request.GET.get('raw',None)=="1":
        return render(request, 'visualizations/base_wordcloud_raw.html',{'string':input_dict['string']})
    return render(request, 'visualizations/base_wordcloud.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict,'iframe':iframe})

def base_triplet_graph(request,input_dict,output_dict,widget,iframe=False):
    if request.GET.get('raw',None)=="1":
        return render(request, 'visualizations/base_triplet_graph_raw.html',{'string':input_dict['string']})
    return render(request, 'visualizations/base_triplet_graph.html',{'widget':widget,'triplets':input_dict['triplets'],'iframe':iframe})
