#from suds.client import Client
import pysimplesoap
from pysimplesoap.client import SoapClient

# class SudsWebService:
    # def __init__(self, wsdl_url):
        # self.client = Client(wsdl_url)
        # self.wsdl_url = wsdl_url
        # self.name = self.client.sd[0].ports[0][0].name
        # self.methods = []
        # for s in self.client.sd:
            # for p in s.ports:
                # for m in p[1]:
                    # method = {}
                    # method['name'] = m[0]
                    # method['inputs'] = []
                   # method['method'] = m
                    # for i in m[1]:
                        # input = {}
                        # input['name']=i[0]
                        # try:
                            # input['type']=i[1].type[0]
                        # except:
                            # pass
                        # method['inputs'].append(input)
                    # self.methods.append(method)
 
    # def __unicode__(self):
        # return self.wsdl_url
    
    # def __str__(self):
        # return self.wsdl_url

        
class WebService:
    def __init__(self, wsdl_url, timeout=60):
        pysimplesoap.client.TIMEOUT = timeout
        self.client = SoapClient(wsdl=wsdl_url,trace=False)
        self.wsdl_url = wsdl_url
        self.name = wsdl_url
        self.methods = []
        for service in self.client.services.values():
            for port in service['ports'].values():
                for op in port['operations'].values():
                    method = {}
                    try:
                        method['documentation']=op['documentation']
                    except:
                        method['documentation']="No documentation provided."
                    method['name']=op['name']
                    method['inputs']=[]
                    method['outputs']=[]
                    input_dict = op['input'].values()[0]
                    for i in input_dict:
                        input = {}
                        input['name']=i
                        input['type']=input_dict[i]
                        method['inputs'].append(input)
                    output_dict = op['output'].values()[0]
                    if type(output_dict)==type([]):
                        output_dict = output_dict[0]
                    for o in output_dict:
                        output = {}
                        output['name']=o
                        method['outputs'].append(output)
                    self.methods.append(method)
    def __unicode__(self):
        return self.wsdl_url
    def __str__(self):
        return self.wsdl_url