import xml.dom.minidom as dom
#import xml.dom.ext.reader.Sax2
#from xml.dom.ext import PrettyPrint

import os
import sys

class XMLCreator(object):
    def __init__(self):
        self.DOMTreeRoot = None
        self.DOMTreeTop = None


    # attributes is a list od tuples: [(attrName, attrValue), ...]
    def addAttributes(self, node, attributes):
        for attr in attributes:
            node.setAttribute(attr[0], attr[1])


    def insertNewNamedTextNode(self, parent, name, value, attributes=[]):
        new = self.DOMTreeTop.createElement(name)
        new.appendChild(self.DOMTreeTop.createTextNode(value))
        if attributes:
            self.addAttributes(new, attributes)
        parent.appendChild(new)


    def insertNewNode(self, parent, name, attributes=[]):
        new = self.DOMTreeTop.createElement(name)
        if attributes:
            self.addAttributes(new, attributes)
        parent.appendChild(new)
        return new



#end class XMLCreator()


#def createXML(filename ='Rules.xml'):
    #outfile = open(filename, 'w')
    #creator = XMLCreator()
    #creator.DOMTreeTop = dom.DOMImplementation().createDocument('', 'MyPMML', None)
    #creator.DOMTreeRoot = creator.DOMTreeTop.documentElement
    #creator.DOMTreeRoot.setAttribute('xmlns', 'http://www.example.org/PMML')

    #rulesBlock = creator.insertNewNode(creator.DOMTreeRoot, 'rules')
    #creator.insertNewNamedTextNode(rulesBlock, 'rule', 'age GE 55 --> virginica')
    #creator.insertNewNamedTextNode(rulesBlock, 'rule', 'tear_rate EQ reduced --> setosa')

    #PrettyPrint(creator.DOMTreeTop, outfile)
    #outfile.close()

#if __name__ == '__main__':

    #createXML("MyRules.xml")


    #xml = creator.DOMTreeTop.toprettyxml(indent='\t', newl='\n', encoding=None)
    #print xml

