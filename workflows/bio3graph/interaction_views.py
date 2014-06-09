from django.shortcuts import render

def bio3graph_filter_integers(request,input_dict,output_dict,widget):
    return render(request, 'interactions/bio3graph_filter_integers.html',{'widget':widget,'intList':input_dict['intList']})



def bio3graph_xml_to_fulltext(request, input_dict, output_dict, widget):
    from NCBI import NCBI_Extractor

    #xmls = input_dict['xml_list']
    # if not isinstance(xmls, list):
    import xml.etree.ElementTree as ET
    #     tree = ET.fromstring(xmls)
    #     #root = tree.getroot()
    #
    #     xml_elements=tree.findall('article')
    #     xmls=[ET.tostring(el) for el in xml_elements]
    #
    file_name = input_dict['xml_file']
    import time
    timea=time.time()
    print file_name

    num_of_all_articles=0
    with open(file_name,'r') as f:
        for line in f:
            if "</article>" in line:
                num_of_all_articles+=1
    print time.time()-timea
    print num_of_all_articles

    article_count=0
    sections = []
    #
    import xml.dom.minidom as dom
    # for xml in xmls:
    #     root = dom.parseString(xml)
    #     sections |= set([sec.getAttribute("sec-type") for sec in root.getElementsByTagName('sec') if sec.hasAttribute('sec-type')])
    def get_title(elem):
        txt=''
        if elem.text:
            txt+=elem.text.strip()
        for child in elem._children: #only one level
            if child.text:
                txt+=child.text.strip()
            if child.tail:
                txt+=child.tail.strip()
        if elem.tail:
            txt+=elem.tail.strip()
        return txt.lower()

    from xml.etree.ElementTree import XMLParser
    parser = XMLParser(encoding="utf-8")
    #optionstree = ET.parse("test.conf", parser=parser)
    if True:
        path=[]
        with open(file_name,'r') as f:
            ignore_title=False

            #with open("D:/diagonalization/glio_aml/domain1/1062151.xml") as f:
            for event, elem in ET.iterparse(f,events=('start','end')):
                if event=="start":
                    if elem.tag == "sec" and not "sec" in path:
                        if 'sec-type' in elem.attrib:
                            sections.append(elem.attrib["sec-type"].lower())
                            ignore_title=True

                    #titles of others
                    elif elem.tag=="title" and not ignore_title and path.count("sec")==1:
                        sections.append(get_title(elem))

                    path.append(elem.tag)
                elif event=="end":
                    if elem.tag=="article":
                        article_count+=1
                        print article_count,"/",num_of_all_articles
                    path.pop()

                    if elem.tag == "sec" and not "sec" in path:
                        ignore_title=False
                elem.clear()
    from collections import Counter
    #print len(sections),sections
    #print Counter(sections)
    section_names= ['article title','abstract','figure captions','table captions']
    if article_count>0:
        section_names+=[a[0]+"::"+str(a[1]) for a in Counter(sections).most_common(None)]


    return render(request, 'visualizations/xml_to_fulltext.html',
                  {'widget': widget, 'section_names': section_names,'num_of_all_articles':num_of_all_articles})


def mesh_filter(request, input_dict, output_dict, widget):
    import json
    from os.path import normpath, join, dirname

    categories=json.load(open(normpath(join(dirname(__file__), 'data/toplevels.json')))) #{'a':['a1','a2','a3'],'b':['b1','b2']}
    return render(request, 'visualizations/mesh_filter.html',   {'categories': categories, 'widget':widget})