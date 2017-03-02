from django.shortcuts import render
import jpype as jp
import common

def weka_local_display_decision_tree(request, input_dict, output_dict, widget):
    """Visualization displaying a decision tree"""

    import subprocess
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir

    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    img_type = 'svg'
    if input_dict['img_type'] == 'raster':
        img_type = 'png'

    classifier = common.deserialize_weka_object(input_dict['classifier'])
    dot_text = classifier.graph()

    filename = '/'.join([str(request.user.id), 'decisionTree-weka-%d.dot' % widget.id])
    destination_dot = '/'.join([MEDIA_ROOT, filename])
    ensure_dir(destination_dot)

    with open(destination_dot, 'w') as dot_file:
        dot_file.write(dot_text)


    # png/svg file
    filename = '/'.join([str(request.user.id),
                         'decisionTree-weka-%d.%s' % (widget.id, img_type)
                         ])
    destination_img = '/'.join([MEDIA_ROOT, filename])
    ensure_dir(destination_img)

    subprocess.call("dot -T%s %s -o %s" % (img_type, destination_dot, destination_img), shell=True)

    return render(request,
                  'visualizations/weka_local_display_decision_tree.html',
                  {'filename': filename,
                   'widget': widget,
                   'input_dict': input_dict})


