'''
NLP visualization views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render
import nlp

def definition_sentences_viewer(request, input_dict, output_dict, widget):
    """
    Parses the input XML and displays the definition sentences given as input.
    
    @author: Anze Vavpetic, 2012
    """
    sentences = nlp.parse_def_sentences(input_dict['candidates']) 
    return render(request, 'visualizations/def_sentences.html',{'widget' : widget, 'sentences' : sentences})

def definition_sentences_viewer2(request, input_dict, output_dict, widget):
    """
    Parses the input XML and displays the definition sentences given as input.
    
    @author: Anze Vavpetic, 2012
    """
    ids_sentence = input_dict["ids_sentence"] == "true"
    ids_article = input_dict["ids_article"] == "true"
    text_sentence = input_dict["text_sentence"] == "true"

    sentences = nlp.parse_def_sentences2(input_dict['candidates'])
    return render(request, 'visualizations/def_sentences2.html',{'widget' : widget, 'sentences' : sentences, 'ids_sentence': ids_sentence, 'ids_article': ids_article, 'text_sentence':text_sentence})


def term_candidate_viewer(request, input_dict, output_dict, widget):
    """
    Parses the input and displays the term candidates.
    
    @author: Anze Vavpetic, 2012
    """
    terms = []
    for line in input_dict['candidates'].split('\n'):
        try:
            score, cand, lemma = line.split('\t')
        except:
            continue
        terms.append({'score' : score, 
                      'cand' : cand.replace('[', '').replace(']',''),
                      'lemma' : lemma.replace('<<', '').replace('>>','')
                      })
    terms = sorted(terms, key = lambda x: x['score'], reverse=True)
    return render(request, 'visualizations/terms.html', {'widget' : widget, 'terms' : terms})

