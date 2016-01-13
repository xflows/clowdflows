from django.contrib import admin

from workflows.models import Category, AbstractWidget, AbstractInput, AbstractOutput, AbstractOption, Workflow

admin.ModelAdmin.save_as = True #adds "Save as new" button to all admin interfaces
admin.ModelAdmin.save_on_top = True #adds admin buttons also on the top of the page
admin.ModelAdmin.search_fields = ['name'] #adds search button on top of admin lists (allways searches by name)


class InputInline(admin.TabularInline):
    model = AbstractInput

class CategoryInline(admin.TabularInline):
    model = Category

class OutputInline(admin.TabularInline):
    model = AbstractOutput

class AbstractWidgetInline(admin.TabularInline):
    model = AbstractWidget

class WidgetAdmin(admin.ModelAdmin):
    inlines = [
        InputInline,
        OutputInline,
    ]
    list_display = ('name','category','package','user')
    
class OptionInline(admin.StackedInline):
    model = AbstractOption
    
class InputAdmin(admin.ModelAdmin):
    inlines = [ OptionInline ]
    list_display = ('name','parameter_type')
    
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