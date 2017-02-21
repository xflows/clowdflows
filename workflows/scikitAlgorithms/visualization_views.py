from django.shortcuts import render

# def scikitAlgorithms_display_summation(request,input_dict,output_dict,widget):
#     if sum(input_dict['intList']) == input_dict['sum']:
#         check = 'The calculation appears correct.'
#     else:
#         check = 'The calculation appears incorrect!'
#     return render(request, 'visualizations/scikitAlgorithms_display_integers.html',{'widget':widget,'input_dict':input_dict, 'output_dict':output_dict, 'check':check})


def scikitAlgorithms_displayDS(request,input_dict,output_dict,widget):
    data = input_dict['data']
    return render(request, 'visualizations/scikitAlgorithms_displayDS.html',{'widget':widget,'input_dict':input_dict,'output_dict':helperDisplayDS(output_dict)})

def scikitDataset_table_to_dict(data):
    attrs, metas, data_new = [], [], []
  #  try:
   #     class_var = data.domain.class_var.name
    #except:
    class_var = ''
    for m in data.domain.get_metas():
        metas.append(data.domain.get_meta(m).name)
    for a in data.domain.attributes:
        attrs.append(a.name)
    pretty_float = lambda x, a: '%.3f' % x if a.var_type == Orange.feature.Type.Continuous else x
    for inst in xrange(len(data)):
        inst_new = []
        for a in data.domain.variables:
            value = data[inst][a.name].value
            inst_new.append((a.name, pretty_float(value, a)))
        for m in data.domain.get_metas():
            value = data[inst][m].value
            a = data.domain.get_meta(m)
            inst_new.append((a.name, pretty_float(value, a)))
        data_new.append(inst_new)
    return {'attrs':attrs, 'metas':metas, 'data':data_new, 'class_var':class_var}


def scikitAlgorithms_displayDecisionTree(request, input_dict, output_dict, widget):
    """Visualization displaying a decision tree"""

    import subprocess
    from sklearn import tree

    # dot_data = StringIO.StringIO()
    dotfile = open("decisionTreeJ48-scikit.dot", 'w')
    tree.export_graphviz(input_dict['classifier'], out_file=dotfile)
    dotfile.close()

    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir

    filename = '/'.join([str(request.user.id), 'decisionTreeJ48-scikit-' + str(widget.id) + '.png'])
    destination = '/'.join([MEDIA_ROOT, filename])
    ensure_dir(destination)

    subprocess.call("dot -Tpng decisionTreeJ48-scikit.dot -o %s" % destination, shell=True)

    return render(request,
                  'visualizations/scikitAlgorithms_display_decision_tree.html',
                  {'filename': filename,
                   'widget': widget,
                   'input_dict': input_dict})
