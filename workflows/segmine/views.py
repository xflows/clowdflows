# helperji, context stvari
from django.shortcuts import render, get_object_or_404, redirect
from django.http import Http404, HttpResponse

# modeli
from workflows.models import *
# auth fore
from django.contrib.auth.decorators import login_required

# Orange bioinformatics differential expression
# from orngBioinformatics.obiExpression import *

# SCORE_METHODS = [("fold change", ExpressionSignificance_FoldChange, twoTailTest, True),
#                  ("log2 fold change", ExpressionSignificance_Log2FoldChange, twoTailTest, True),
#                  ("t-test", ExpressionSignificance_TTest_T, twoTailTest, True),
#                  ("t-test p-value", ExpressionSignificance_TTest_PValue, oneTailTestLow, True),
#                  ("anova", ExpressionSignificance_ANOVA_F, oneTailTestHi, False),
#                  ("anova p-value", ExpressionSignificance_ANOVA_PValue, oneTailTestLow, False),
#                  ("signal to noise ratio", ExpressionSignificance_SignalToNoise, twoTailTest, True),
#                  ("info gain", ExpressionSignificance_Info, oneTailTestHi, True),
#                  ("chi-square", ExpressionSignificance_ChiSquare, oneTailTestHi, True),
#                  ("mann-whitney", ExpressionSignigicance_MannWhitneyu_U, oneTailTestLow, True),
#                  ("AREA (timeseries)", ExpressionSignificance_AREA, oneTailTestHi, False),
#                  ("FC (timeseries)", ExpressionSignificance_FCts, oneTailTestHi, False)]

@login_required
def get_new_feature_selection_scores(request, widget_id, method = 0):
    w = get_object_or_404(Widget, pk=widget_id)
    if w.workflow.user == request.user:
        data = w.inputs.all()[0]
        _, score_method, _, twotailtest = SCORE_METHODS[method]
        return HttpResponse(data, mimetype='text/json')
    else:
        return HttpResponse(status=400)
