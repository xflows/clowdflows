# Segs web service interface
# 
# anze.vavpetic@ijs.si, 2013
import time
import json

def recursive_asdict(d):
    from suds.sudsobject import asdict
    from suds.sax.text import Text
    """Convert Suds object into serializable format."""
    out = {}
    for k, v in asdict(d).iteritems():
        if hasattr(v, '__keylist__'):
            out[k] = recursive_asdict(v)
        elif isinstance(v, list):
            out[k] = []
            for item in v:
                if hasattr(item, '__keylist__'):
                    out[k].append(recursive_asdict(item))
                else:
                    if isinstance(item, Text):
                        item = str(item)
                    out[k].append(item)
        else:
            if isinstance(v, Text):
                v = str(v)
            out[k] = v
    return out

class Segs:
    '''Interface to the SEGS web service'''

    def __init__(self, wsdl):
        from suds.client import Client
        self.wsdl = wsdl
        self.client = Client(self.wsdl)

    def build_inputs(self, inputs):
        fmt_inputs = {}
        for input_name, input_value in inputs.items():
            if input_name == 'inputData':
                fmt_inputs[input_name] = [{'geneID' : gene, 'rank' : rank} 
                                          for gene, rank in inputs['inputData']]
            else:
                if input_value in ['true', 'false']:
                    input_value = bool(input_value == 'true')
                fmt_inputs[input_name] = input_value
        return fmt_inputs

    def run(self, inputs, widget=None):
        inputs = self.build_inputs(inputs)
        jobID = self.client.service.runSEGS(**inputs)
        self.wait_for_job(widget, jobID)
        rules = self.client.service.getResult(jobID=jobID)
        return recursive_asdict(rules)

    def wait_for_job(self, widget, jobID):
        if widget:
            widget.progress = 0
            widget.save()
            while widget.progress < 100:
                widget.progress = self.client.service.getProgress(jobID=jobID)
                widget.save()
                time.sleep(2)
            widget.progress = 100
            widget.save()
        else:
            progress = 0
            while progress < 100:
                progress = self.client.service.getProgress(jobID=jobID)
                time.sleep(2)

if __name__ == '__main__':
    inputs = {
         'inputData' : [('915', 0.49025347828865051), ('3122', 0.37040603160858154), ('972', 0.32695004343986511), ('3932', 0.31558018922805786), ('5588', 0.3136667013168335), ('4925', 0.29072445631027222), ('919', 0.28687822818756104), ('4068', 0.28160524368286133), ('4118', 0.25243601202964783), ('3115', 0.24201515316963196), ('6932', 0.2408420592546463), ('6964', 0.22654871642589569), ('3113', 0.22505617141723633), ('3108', 0.22324635088443756), ('60481', 0.21331347525119781), ('6001', 0.21196004748344421), ('9760', 0.20667321979999542), ('51561', 0.20581072568893433), ('11343', 0.20454806089401245), ('5778', 0.20379079878330231), ('8531', 0.20356714725494385), ('7535', 0.19858548045158386), ('8073', 0.19376169145107269), ('917', 0.19196289777755737), ('50852', 0.19075927138328552), ('9452', 0.18605667352676392), ('7805', 0.18238823115825653), ('961', 0.18072117865085602), ('27040', 0.1805538684129715), ('2752', 0.17970409989356995), ('4854', 0.17267157137393951), ('11034', 0.16996371746063232), ('10868', 0.16942119598388672), ('10962', 0.1689973771572113), ('6415', 0.16816037893295288), ('9744', 0.16780166327953339), ('10916', 0.1672971248626709), ('100133941', 0.16565148532390594), ('445347', 0.16510523855686188), ('26191', 0.16326169669628143), ('1075', 0.16308797895908356), ('6643', 0.16152414679527283), ('8418', 0.16110159456729889), ('51635', 0.16072908043861389), ('7216', 0.16067247092723846), ('923', 0.16051796078681946), ('57711', 0.15992861986160278), ('1047', 0.1580720841884613), ('7419', 0.15326650440692902), ('2932', 0.15245367586612701), ('10493', 0.15039806067943573), ('3983', 0.14954331517219543), ('55556', 0.14651331305503845), ('4689', 0.14639928936958313), ('199', 0.1459343433380127), ('9363', 0.14583694934844971), ('55966', 0.14577805995941162), ('2026', 0.14299096167087555), ('1353', 0.14258190989494324), ('3127', 0.14167901873588562), ('6721', 0.14146885275840759), ('9498', 0.13927952945232391), ('23173', 0.13891413807868958), ('10123', 0.13890625536441803), ('4240', 0.13865220546722412), ('3709', 0.13837365806102753), ('6307', 0.13809692859649658), ('9743', 0.13666900992393494), ('268', 0.13571131229400635), ('9473', 0.1354944109916687), ('7102', 0.13512954115867615), ('6585', 0.13492228090763092), ('23654', 0.13485057651996613), ('22920', 0.1346772313117981), ('9315', 0.13407155871391296), ('9781', 0.13341896235942841), ('6942', 0.1332969069480896), ('8835', 0.13296535611152649), ('8671', 0.13190098106861115), ('10007', 0.13187795877456665), ('3992', 0.13028360903263092), ('9852', 0.13013292849063873), ('3156', 0.12988860905170441), ('7867', 0.12937329709529877), ('930', 0.12887421250343323), ('8857', 0.12884140014648438), ('653354', 0.12865832448005676), ('5825', 0.12728548049926758), ('4047', 0.12721861898899078), ('11161', 0.12719236314296722), ('11165', 0.12719236314296722), ('11164', 0.12719236314296722), ('11163', 0.12719236314296722), ('11162', 0.12719236314296722)],
         'useKEGG' : False,
         'cutoff' : 45
    }
    segs = Segs('http://segs.ijs.si:8090/SEGS?wsdl')
    result = segs.run(inputs)
    with open('rules.pkl', 'w') as f:
        json.dump(result, f)
        f.flush()
    print result