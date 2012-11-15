import string
from django.core.management.base import BaseCommand, CommandError
from workflows.models import Category, AbstractWidget, AbstractInput, AbstractOutput, AbstractOption
from django.core import serializers
from collections import Counter
from optparse import make_option
from pprint import pprint
import sys

class Command(BaseCommand):
    args = 'file_name top_level_category'
    help = 'Reverse engineering of C# function attributes based on current database definitions'

    def handle(self, *args, **options):
        if (len(args)<2):
            raise CommandError('Arguments "file_name" and "top_level_category" are required!')

        try:
            f = open(args[0], 'w')
        except:
            raise CommandError('There was a problem with creating/overwriting given output file')

        template = '{ind}public class Category {{\n'\
                   '{cats}\n'\
                   '{ind}}}\n'\
                   '{ind}\n'\
                   '{ind}//----------------------------------------------------\n'\
                   '{ind}\n'\
                   '{wids}'

        topcats = Category.objects.filter(name = args[1])
        if topcats.count()==0:
            raise CommandError('Given top_level_category name "%s" was not found in the database'%args[1])

        catDefStr = ""
        widDefStr = ""
        mappingDict = dict()
        self.stdout.write('Collecting category hierarchy and included widgets.\n')
        ord = 0
        for topcat in topcats:
            ord += 1
            catDefStr += self.getCategoryCSharpDef(topcat, mappingDict, ord, "Category", 2)
        self.stdout.write(' done.\n')

        self.stdout.write('Optputing c# representations for models.\n')
        for cat in mappingDict:
            catType = mappingDict[cat]
            catObj = Category.objects.filter(id = cat)[0]
            ord = 0
            for wid in catObj.widgets.all().order_by("order"):
                ord += 1
                widDefStr += self.getWidgetCSharpDef(wid,catType,ord,1)
        self.stdout.write(' done.\n')

        outstr = template.format(
            ind = "    "*1,
            cats = catDefStr,
            wids = widDefStr
        )
        #self.stdout.write('PAUSING... press any key to continue!\n')
        #char = sys.stdin.read(1)

        try:
            f.write(outstr)
        except:
            raise CommandError('There was a problem with writing to the given output file')

        self.stdout.write('Reverse engineering successfully finished. Results written to the file.\n')

    def getCategoryCSharpDef(self, cat, mappingDict, order, superName="", indentLevel=1):
        self.stdout.write('.')
        template = '{ind}public class {fName} : IntfCategory {{\n' \
                   '{ind}    public {fName}() {{\n' \
                   '{ind}        Name = "{name}";\n' \
                   '{ind}        Uid = "{uid}";\n' \
                   '{ind}        Order = {order};\n' \
                   '{ind}    }}\n' \
                   '{sub}' \
                   '{ind}}}\n'

        fName = ''.join(ch for ch in cat.name if ch.isalpha())
        fNameSuper = (superName+'.'+fName).strip('.')
        mappingDict[cat.id] = fNameSuper

        substr = ""
        ord = 0
        for subcat in Category.objects.filter(parent = cat.id).order_by("order"):
            ord += 1
            substr += self.getCategoryCSharpDef(subcat, mappingDict, ord, fNameSuper, indentLevel+1)

        return template.format(
            ind = "    "*indentLevel,
            fName = fName,
            name = cat.name,
            uid = cat.uid,
            order = order,
            sub = substr
        )

    def getWidgetCSharpDef(self, wid, catType, order, indentLevel=1):
        breakLine = "\n"+("    "*(indentLevel+1))
        breakLineNext = "\n"+("    "*(indentLevel+2))

        self.stdout.write('.')

        template = '{ind}#region Widget definition\n' \
            '{ind}[IntfWidget({uid}, typeof({catType}),{br}{name}, {image}, Order = {order}{additional})]\n{inputs}{outputs}' \
            '{ind}#endregion\n' \
            '{ind}public void '+wid.action+'() {{}}\n' \
            '\n'
        additional = ''
        additional += self.FormatStringIfExists(breakLine+'InteractionView', wid.interaction_view)
        additional += self.FormatStringIfExists(breakLine+'PostInteractAction', wid.post_interact_action)
        additional += self.FormatStringIfExists(breakLine+'VisualizationView', wid.visualization_view)
        additional += self.FormatBoolIfTrue(breakLine+'HasProgressBar', wid.has_progress_bar)
        additional += self.FormatDescriptionIfExists(breakLine+'Description', wid.description, breakLineNext)

        inputs = ''
        ord = 0
        for inp in wid.inputs.all().order_by("order"):
            ord += 1
            inputs += self.getInputCSharpDef(inp, ord, indentLevel)

        outputs = ''
        ord = 0
        for out in wid.outputs.all().order_by("order"):
            ord += 1
            outputs += self.getOutputCSharpDef(out, ord, indentLevel)

        return template.format(
            ind = "    "*indentLevel,
            uid = self.FormatCSharpString(wid.uid),
            catType = catType,
            name = self.FormatCSharpString(wid.name),
            image = self.FormatCSharpString(wid.static_image),
            order = order,
            additional = additional,
            inputs = inputs,
            outputs = outputs,
            br = breakLine
        )

    def getInputCSharpDef(self, inp, order, indentLevel=1):
        template = '{ind}[IntfInput({param}, {uid}, {name}, {shortName}, Order = {order}{additional})]\n'

        breakLine = "\n"+("    "*(indentLevel+1))
        breakLineNext = "\n"+("    "*(indentLevel+2))

        self.stdout.write('.')

        additional = ''
        additional += self.FormatBoolIfTrue('Required', inp.required)

        #inp = AbstractInput()
        add1 = ''
        add1 += self.FormatBoolIfTrue('Parameter', inp.parameter)
        add1 += self.FormatBoolIfTrue('Multi', inp.multi)
        add1 += self.FormatDescriptionIfExists('Default', inp.default, breakLineNext)
        add1 += self.FormatEnumIfExists('ParameterType', inp.parameter_type, 'ParameterType', '')
        if len(add1) > 0:
            add1 = ", " + breakLine + add1[2:]

        additional += add1
        additional += self.FormatDescriptionIfExists(breakLine+'Description', inp.description, breakLineNext)
        additional += self.FormatStringArrayIfExists(breakLine+'SelectBoxValues', self.GetSelectBoxValues(inp, breakLineNext))

        return template.format(
            ind = "    "*indentLevel,
            param = self.FormatCSharpString(inp.variable),
            uid = '"#in'+str(order)+'"' , #self.FormatCSharpString(inp.uid),
            name = self.FormatCSharpString(inp.name),
            shortName = self.FormatCSharpString(inp.short_name),
            order = order,
            additional = additional,
            br = breakLine
        )

    def GetSelectBoxValues(self, inp, breakLineNext):
        template = '{ind}@"{val} | {name} | {uid}",'
        self.stdout.write('.')

        inputs = ''
        ord = 0
        for opt in inp.options.all():
            ord += 1
            inputs += template.format(
                ind = breakLineNext,
                uid = '#opt'+str(ord), #self.FormatCSharpString(opt.uid),
                name = string.replace(opt.name,'"','""'),
                val = opt.value,
            )
        return inputs

    def getOutputCSharpDef(self, outp, order, indentLevel=1):
        template = '{ind}[IntfOutput({var}, {uid}, {name}, {shortName}, Order = {order}{additional})]\n'

        breakLine = "\n"+("    "*(indentLevel+1))
        breakLineNext = "\n"+("    "*(indentLevel+2))

        self.stdout.write('.')

        #outp = AbstractOutput()
        additional = ''
        additional += self.FormatDescriptionIfExists(breakLine+'Description', outp.description, breakLineNext)

        return template.format(
            ind = "    "*indentLevel,
            var = self.FormatCSharpString(outp.variable),
            uid = '"#out'+str(order)+'"' , #self.FormatCSharpString(outp.uid),
            name = self.FormatCSharpString(outp.name),
            shortName = self.FormatCSharpString(outp.short_name),
            order = order,
            additional = additional
        )


    def IsNotNull(self, value):
        return (value != None and len(value) > 0)
    def FormatCSharpString(self, value):
        if self.IsNotNull(value):
            return '"'+string.strip(value)+'"'
        else:
            return 'null'
    def FormatStringIfExists(self, name, val):
        if self.IsNotNull(val):
            return ', {name} = "{val}"'.format(name = name, val = string.replace(string.strip(val),'"','\\"'))
        return ''
    def FormatEnumIfExists(self, name, val, enum, nullVal):
        if self.IsNotNull(val) and val != nullVal:
            return ', {name} = {enum}.{val}'.format(name = name, val = val, enum = enum)
        return ''
    def FormatStringArrayIfExists(self, name, val):
        if self.IsNotNull(val):
            return ', {name} = new string[] {{{val}}}'.format(name = name, val = val)
        return ''
    def FormatBoolIfTrue(self, name, val):
        if val:
            return ', {name} = {val}'.format(name = name, val = string.lower(str(val)))
        return ''
    def FormatDescriptionIfExists(self, name, val, breakLine):
        ret = ''
        val = string.replace(string.strip(val),'"','""')
        width = 80
        if self.IsNotNull(val):
            while len(val)>0:
                if len(val)>width+4 and string.find(val, ' ',width)>0:
                    spaceIndex = string.find(val, ' ',width)
                    ret += string.replace(val[:spaceIndex+1],'\r\n','" + "\\r\\n" + ' + breakLine + '@"') + '" +' + breakLine + '@"'
                    val = val[spaceIndex+1:]
                else:
                    ret += string.replace(val,'\r\n','" + ' + breakLine + '@"')
                    val = ""
            ret = ', {name} = @"{val}"'.format(name = name, val = ret)
        return ret


