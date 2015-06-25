from django.shortcuts import render

def MUSE_v3_filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/MUSE_v3_filter_integers.html',{'widget':widget,'intList':input_dict['intList']})



def MUSE_string_to_file_V3(request, input_dict, output_dict, widget):
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir

    basename = '/'.join([str(request.user.id), str(widget.id) + str(input_dict['fending'])])
    destination = '/'.join([MEDIA_ROOT, basename])
    ensure_dir(destination)
    with open(destination, 'w') as f:
        f.write(str(input_dict['data']))
    return render(request, 'visualizations/MUSE_string_to_file_v3.html', {'widget': widget, 'fileURL': basename})

