from django.conf.urls import patterns, include, url

packageUrls = {}
from workflows import module_importer
def set_package_url(name, value, package):
    if name == 'urlpatterns':
        packageUrls[package] = value
module_importer.import_all_packages_libs("urls",set_package_url)

urlpatterns = patterns('',)

for pck in  packageUrls:
    urlpatterns += patterns('',url(r'^'+pck+'/', include(packageUrls[pck])),)

urlpatterns += patterns('',
    url(r'^$', 'workflows.views.index', name='the index'),
    url(r'^new-workflow/$', 'workflows.views.new_workflow', name='new workflow'),
    url(r'^add-widget/$', 'workflows.views.add_widget', name='add widget'),
    url(r'^save-position/', 'workflows.views.save_position', name='save position'),
    url(r'^add-connection/', 'workflows.views.add_connection', name='add connection'),
    url(r'^delete-widget/', 'workflows.views.delete_widget', name='delete widget'),
    url(r'^delete-workflow/', 'workflows.views.delete_workflow', name='delete workflow'),
    url(r'^delete-connection/', 'workflows.views.delete_connection', name='delete connection'),
    url(r'^add-subprocess/', 'workflows.views.add_subprocess', name='add subprocess'),
    url(r'^get-subprocess/', 'workflows.views.get_subprocess', name='get subprocess'),
    url(r'^add-input/', 'workflows.views.add_input', name='add input'),
    url(r'^add-output/', 'workflows.views.add_output', name='add output'),
    url(r'^add-for/', 'workflows.views.add_for', name='add for'),
    url(r'^add-cv/', 'workflows.views.add_cv', name='add cv'),
    url(r'^synchronize-widgets/', 'workflows.views.synchronize_widgets', name='synchronize widgets'),
    url(r'^synchronize-connections/', 'workflows.views.synchronize_connections', name='synchronize connections'),
    url(r'^get-widget/', 'workflows.views.get_widget', name='get widget'),
    url(r'^get-parameters/', 'workflows.views.get_parameters', name='get parameters'),
    url(r'^save-parameter/', 'workflows.views.save_parameter', name='save parameter'),
    url(r'^get-configuration/', 'workflows.views.get_configuration', name='get configuration'),
    url(r'^save-configuration/', 'workflows.views.save_configuration', name='save configuration'),
    url(r'^get-rename/', 'workflows.views.get_rename_dialog', name='rename widget dialog'),
    url(r'^rename-widget/', 'workflows.views.rename_widget', name='rename widget'),
    url(r'^rename-workflow/', 'workflows.views.rename_workflow', name='rename workflow'),
    url(r'^run-widget/', 'workflows.views.run_widget', name='run widget'),
    url(r'^widget-results/', 'workflows.views.widget_results', name='widget results'),
    url(r'^widget-visualization/', 'workflows.views.visualize_widget', name='widget visualization'),
    url(r'^widget-iframe/(?P<widget_id>[0-9]+)/$', 'workflows.views.widget_iframe', name='widget iframe'),
    url(r'^get-unfinished/', 'workflows.views.get_unfinished', name='get unfinished'),
    url(r'^upload-handler/$', 'workflows.views.upload_handler', name='file upload'),
    url(r'^widget-interaction/', 'workflows.views.widget_interaction', name='widget interaction'),
    url(r'^finish-interaction/', 'workflows.views.finish_interaction', name='finish interaction'),
    url(r'^import-webservice/', 'workflows.views.import_webservice', name='import webservice'),

    url(r'^documentation/', 'workflows.views.documentation', name='documentation'),

    url(r'^get-designate-dialogs/', 'workflows.views.get_designate_dialogs', name='get designate dialogs'),

    url(r'^save-designation/', 'workflows.views.save_designation', name='save designation'),

    url(r'^get-category/', 'workflows.views.get_category', name='get category'),

    url(r'^widget-progress/', 'workflows.views.widget_progress', name='widget progress'),

    url(r'^copy-workflow/(?P<workflow_id>[0-9]+)/$', 'workflows.views.copy_workflow', name='copy workflow'),

    url(r'^workflow-url/', 'workflows.views.workflow_url', name='workflow url'),

    url(r'^unfinish-visualizations/', 'workflows.views.unfinish_visualizations', name='unfinish visualizations'),

    url(r'^(?P<workflow_id>[0-9]+)/$', 'workflows.views.open_workflow', name='open workflow'),

    url(r'^reset-widget/', 'workflows.views.reset_widget', name='reset widget'),
    url(r'^get-executed-status/', 'workflows.views.get_executed_status', name='get executed status'),
    url(r'^reset-workflow/', 'workflows.views.reset_workflow', name='reset workflow'),

    url(r'^export-package/(?P<packages>.+)/$', 'workflows.views.export_package', name='export_package'),

    url(r'^widget-inputs/(?P<widget_id>[0-9]+)/$', 'workflows.views.widget_inputs', name='widget inputs'),

    url(r'^workflow_results/(?P<workflow_id>[0-9]+)/$', 'workflows.views.workflow_results', name='workflow_results'),

)
