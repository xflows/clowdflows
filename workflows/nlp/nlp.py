'''
NLP common functions.

@author: Anze Vavpetic, 2012
'''
import xml.etree.ElementTree as xml
import re


def parse_def_sentences(sentsXML):
    """
    Parses the candidate definition sentences from the input XML string.
    """
    dom = xml.fromstring(sentsXML)
    sents = dom.findall('S')               # Lists all the tagged sentences
    sentences = []
    for sent in sents:
        sentences.append({'id' : sent.attrib['sid_sp'], 'aid' : sent.attrib['aid_sp'], 'defvalue' : sent.attrib['defvalue'], 'txt' : sent.text.strip()})
    sentences = sorted(sentences, key = lambda x: x['id'])
    return sentences


def parse_def_sentences2(sentsXML):
    """
    Parses the candidate definition sentences from the input XML string.
    """
    dom = xml.fromstring(sentsXML)
    sents = dom.findall('S')               # Lists all the tagged sentences
    sentences = {}
    for sent in sents:
        text = sent.text.strip().replace(" ,", ",").replace(" .", ".").replace(" !", "!").replace(" ?", "?").replace("( ", "(").replace(" )", ")").replace("[ ", "[").replace(" ]", "]")
        sentences[text] = {'id' : sent.attrib['id'], 'aid' : sent.attrib['aid'], 'txt' : text}
    sentences = sorted(sentences.values(), key = lambda x: x['id'])
    return sentences

def sentences_to_xml(sentences):
    root = xml.Element('definitions')
    for sent in sentences:
        el = xml.Element('S', attrib={'sid_sp' : sent['id'], 'aid_sp' : sent['aid'], 'defvalue' : sent['defvalue']})
        el.text = sent['txt']
        root.append(el)
    return xml.tostring(root, "UTF-8")


def sentences_to_xml2(sentences):
    root = xml.Element('definitions')
    for sent in sentences:
        el = xml.Element('S', attrib={'id' : sent['id'], 'aid' : sent['aid']})
        el.text = sent['txt']
        root.append(el)
    return xml.tostring(root, "UTF-8")
    

if __name__ == '__main__':
    # Run test
    pats = open(r'D:\programiranje\Glossary\patterns2.txt').read()
    sents = parse_def_sentences(pats)
    xml = sentences_to_xml(sents)
    print xml