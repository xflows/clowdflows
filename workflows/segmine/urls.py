from django.conf.urls.defaults import patterns, include, url

urlpatterns = patterns('',
    url(r'^get-new-feature-selection-scores/widget(?P<widget_id>[0-9]+)/(?P<method>\w+)/$', 'workflows.segmine.views.get_new_feature_selection_scores', 
        name='get new feature selection scores'),
)