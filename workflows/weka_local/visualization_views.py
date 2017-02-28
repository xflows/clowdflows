from django.shortcuts import render
import jpype as jp
import common

def weka_local_display_decision_tree(request, input_dict, output_dict, widget):
    """Visualization displaying a decision tree"""

    import subprocess

    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    img_type = 'svg'
    if input_dict['img_type'] == 'raster':
        img_type = 'png'

    classifier = common.deserialize_weka_object(input_dict['classifier'])
    dot_text = classifier.graph()

    with open("decisionTree-weka.dot", 'w') as dot_file:
        dot_file.write(dot_text)

    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir

    filename = '/'.join([str(request.user.id),
                         'decisionTree-weka-%d.%s' % (widget.id, img_type)
                         ])
    destination = '/'.join([MEDIA_ROOT, filename])
    ensure_dir(destination)

    subprocess.call("dot -T%s decisionTree-weka.dot -o %s" % (img_type, destination), shell=True)

    return render(request,
                  'visualizations/weka_local_display_decision_tree.html',
                  {'filename': filename,
                   'widget': widget,
                   'input_dict': input_dict})
