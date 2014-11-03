'''
Segmine interaction viewes.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render


def segmine_ttest_gene_filter(request, input_dict, output_dict, widget):
    dataset = input_dict['dataset']
    targets = dataset.domain.classVar.values
    return render(request, 'interactions/segmine_ttest_gene_filter.html', 
                  {'widget': widget, 'targets': targets,
                   'default_threshold': 0.05})


def segmine_fc_gene_filter(request, input_dict, output_dict, widget):
    dataset = input_dict['dataset']
    targets = dataset.domain.classVar.values
    return render(request, 'interactions/segmine_fc_gene_filter.html', 
                  {'widget':widget, 'targets' : targets, 
                   'default_threshold': 1.0})


def segmine_rule_browser(request, input_dict, output_dict, widget):
    rules = input_dict['rules']
    aggregate = rules[0].has_key('aggregate_pval') if rules else False
    return render(request, 'interactions/segmine_rule_browser.html', 
                  {'widget': widget, 'rules': rules, 'aggregate': aggregate})


def segmine_hclustering(request, input_dict, output_dict, widget):
    import json
    import Orange
    import constants

    def assign_numbers(root):
        nodes = Orange.clustering.hierarchical.preorder(root)
        for (i, node) in enumerate(nodes):
            node.setattr('idx', i)

    def to_dict(cluster):
        if cluster.branches:
            return {"name": cluster.idx, "children": [to_dict(x) for x in cluster.branches]}
        else:
            # stupid segmine hardcoded meta attribute
            if cluster[0].has_meta(constants.NAME_ATTR):
                return {"name": str(cluster[0][constants.NAME_ATTR])}
            else:
                return {"name": str(cluster[0])}

    def hclustering2json(cluster):
        return json.dumps(to_dict(cluster))


    hcluster = input_dict['hclustering']
    assign_numbers(hcluster)
    djson = hclustering2json(hcluster)
    return render(request, 'interactions/segmine_hierarchical_clustering.html', {'widget': widget,
                                                                                 'dendrogram_json': djson,
                                                                                 'nexamples': len(hcluster)})