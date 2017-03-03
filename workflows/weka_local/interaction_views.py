from django.shortcuts import render
import common
import jpype as jp

def weka_local_arff_to_weka_instances(request,input_dict,output_dict,widget):
    if not jp.isThreadAttachedToJVM():
        jp.attachThreadToJVM()

    tmp = common.TemporaryFile(suffix='.arff')
    tmp.writeString(input_dict['arff'])

    source = jp.JClass('weka.core.converters.ConverterUtils$DataSource')(tmp.name)
    instances = source.getDataSet()

    instances.setClassIndex(-1)

    att_class = jp.JClass('weka.core.Attribute')
    list_atts = []
    for i in range(instances.numAttributes()):
        if instances.attribute(i).type() == att_class.NUMERIC:
            tup = (i, "%s (NUMERIC)" % (instances.attribute(i).name()) )
        else:
            tup = (i, "%s (CATEGORICAL)" % (instances.attribute(i).name()) )
        list_atts.append(tup)

    return render(request,
                  'interactions/arff_to_weka_instances.html',
                  {'widget':widget,'list_atts':list_atts})
