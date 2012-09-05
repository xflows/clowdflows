from pysimplesoap.client import SoapClient

def inspect(wsdl_url):
    client = SoapClient(wsdl=wsdl_url,trace=False)
    print "Target Namespace", client.namespace
    for service in client.services.values():
        for port in service['ports'].values():
            print port['location']
            for op in port['operations'].values():
                print 'Name:', op['name']
                print 'Docs:', op['documentation'].strip()
                print 'SOAPAction:', op['action']
                print 'Input', op['input'] # args type declaration
                print 'Output', op['output'] # returns type declaration
                print '\n'