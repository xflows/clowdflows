'''
Decision support interaction views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render


def decision_support_wsm(request, input_dict, output_dict, widget):
    from wsm import WeightedSumModel
    data = input_dict['data']
    model = WeightedSumModel(data)
    attributes = sorted(model.ranges.items())
    unusable = sorted(model.unusable)
    return render(request, 'interactions/wsm.html',
                  {'widget': widget, 'attributes': attributes,
                   'unusable_attributes': unusable})
