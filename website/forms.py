from django import forms
import json
from workflows.models import AbstractWidget

class ImportForm(forms.Form):
    data = forms.CharField(widget=forms.Textarea(attrs={'class':'formfieldclass'}))

    def clean_data(self):

        def get_all_abstract_widgets(jsondata,awset):
            widgets = jsondata.get('widgets',None)
            for w in widgets:
                if w['abstract_widget']:
                    awset.add((w['abstract_widget'],w['abstract_widget_package'],w['name']))
                if w['workflow']:
                    awset = get_all_abstract_widgets(w['workflow'],awset)
            return awset


        data = self.cleaned_data['data']
        try:
            d = json.loads(data)
        except:
            raise forms.ValidationError("Please enter valid json.")

        try:
            abstract_widgets = get_all_abstract_widgets(d,set())
        except:
            raise forms.ValidationError("Please enter valid workflow data.")

        abstract_widgets = list(abstract_widgets)

        for aw in abstract_widgets:
            if AbstractWidget.objects.filter(uid=aw[0],package=aw[1]).count()==0:
                raise forms.ValidationError("The widget "+aw[2]+" from tha package "+aw[1]+" is missing in this installation. Cannot import!")
        return data