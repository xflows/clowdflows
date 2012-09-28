from workflows.models import Category, AbstractWidget, AbstractInput, AbstractOutput, AbstractOption, Workflow
from django.contrib import admin
from django.forms import TextInput, Textarea
from django.db import models

from orderable_inlines import OrderableTabularInline

class OrderableTabularInlineStyled(OrderableTabularInline):
    formfield_overrides = {
        models.CharField: {'widget': TextInput(attrs={'size':10})},
        models.TextField: {'widget': Textarea(attrs={'rows':1, 'cols':20})},
        }
    orderable_field = 'order'

class InputInline(OrderableTabularInlineStyled):
    model = AbstractInput

class CategoryInline(OrderableTabularInlineStyled):
    model = Category

class OutputInline(OrderableTabularInlineStyled):
    model = AbstractOutput

class AbstractWidgetInline(OrderableTabularInlineStyled):
    model = AbstractWidget

class WidgetAdmin(admin.ModelAdmin):
    inlines = [
        InputInline,
        OutputInline,
    ]
    list_display = ('name','category','user')
    
class OptionInline(admin.StackedInline):
    model = AbstractOption
    
class InputAdmin(admin.ModelAdmin):
    inlines = [ OptionInline ]
    
class WorkflowAdmin(admin.ModelAdmin):
    list_display = ('name','user','public')
    
class CategoryAdmin(admin.ModelAdmin):
    list_display = ('name','user')
    inlines = [
        AbstractWidgetInline,
        CategoryInline
    ]

#admin.site.register(UserProfile)
admin.site.register(Category,CategoryAdmin)
admin.site.register(AbstractWidget,WidgetAdmin)
admin.site.register(AbstractInput,InputAdmin)
admin.site.register(Workflow, WorkflowAdmin)
#admin.site.register(AbstractOutput)