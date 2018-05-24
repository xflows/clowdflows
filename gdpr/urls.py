from django.conf.urls import patterns, include, url

urlpatterns = patterns(
    '',
    url(r'^gdpr/$', 'gdpr.views.gdpr_form', name='gdpr form'),
)
