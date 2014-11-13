from django.shortcuts import render
import os


def MUSE_display_summation(request,input_dict,output_dict,widget):
    if sum(input_dict['intList']) == input_dict['sum']:
        check = 'The calculation appears correct.'
    else:
        check = 'The calculation appears incorrect!'
    return render(request, 'visualizations/MUSE_display_integers.html',{'widget':widget,'input_dict':input_dict, 'output_dict':output_dict, 'check':check})



def MUSE_view_xmlOLD(request, input_dict, output_dict, widget):
    return render(request, 'visualizations/MUSE_view_xml.html',
                  {'widget': widget, 'xml_data': input_dict['xml_data']})


def MUSE_view_xml(request, input_dict, output_dict, widget):
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir

    filename = '/'.join([str(request.user.id), str(widget.id) + '.xml'])
    destination = '/'.join([MEDIA_ROOT, filename])
    ensure_dir(destination)
    f = open(destination, 'w')
    f.write(str(input_dict['xml_data']))
    f.close()

    return render(request, 'visualizations/MUSE_view_xml.html',
                  {'widget': widget, 'filename': filename})


def MUSE_virtual_environment_visualization(request, input_dict, output_dict, widget):
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir
    filename = os.path.join(str(request.user.id), str(widget.id) + '.txt')
    destination = os.path.join(MEDIA_ROOT, filename)
    ensure_dir(destination)
    f = open(destination, 'w')
    f.write(str(input_dict['NLP_data']))
    f.close()
    return render(request,
                  'visualizations/MUSE_view_3D_environment.html',
                  {'widget': widget,
                   'filename': filename,
                   'unitylink': input_dict['unitylink']
                   })


def MUSE_virtual_environment_demonstrator_tuk(request, input_dict, output_dict, widget):
    from urllib import urlencode
    params = urlencode({'filename': str(input_dict['mappingLink'])})
    unityLink = '%s?%s' % (str(input_dict['unityLink']), params)
    # print unityLink
    return render(request,
                  'visualizations/MUSE_Tuk_demonstrator.html',
                  {'widget': widget,
                   'unityLink': unityLink}
                  )


def MUSE_virtual_environment_demonstrator_tuk_local(request, input_dict, output_dict, widget):
    from urllib import urlencode
    params = urlencode({'filename': str(input_dict['mappingLink'])})
    unityLink = '%s?%s' % (str(input_dict['unityLink']), params)
    # print unityLink
    return render(request,
                  'visualizations/MUSE_Tuk_demonstrator_local.html',
                  {'widget': widget,
                   'unityLink': unityLink}
                  )
