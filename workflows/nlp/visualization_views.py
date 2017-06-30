'''
NLP visualization views.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''
from django.shortcuts import render
import nlp
import nltk
from nltk.collocations import *
import pandas
import numpy as np

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


def display_corpus_statistic(request, input_dict, output_dict, widget, narrow_doc='n'):
  
    corpus = input_dict['corpus']
    stat_type = input_dict['stat_type']
    allAnnotations = 0
    result_list = []
    n = int(input_dict['n_gram'])

    #get some general stats
    general_stats = {}
    general_stats['num_doc'] = len(corpus)
    doc_lengths = []
    all_tokens = set()
    for doc in corpus:
        doc = doc.split()
        doc_lengths.append(len(doc))
        for tok in doc:
            all_tokens.add(tok)
    general_stats['num_tokens'] = sum(doc_lengths)
    general_stats['avg_doc_length'] = float(general_stats['num_tokens'])/general_stats['num_doc']
    general_stats['ttr'] = len(all_tokens)/float(general_stats['num_tokens'])

    if stat_type == 'frequency' or stat_type == 'dis_legomena' or stat_type == 'hapax_legomena':
        annotation_dict = {}
        for doc in corpus:
            if doc.count('###') > 3:
                annotations = doc.split('###')
            else:
                annotations = doc.split()
            length = len(annotations)
            for i in range(0, length - n + 1):
                combo = ""
                for j in range(i, i + n):
                    value = annotations[j]
                    if j > i:
                        combo += " "
                    combo += value
                            
                if len(combo) > 0:
                    allAnnotations += 1
                    if combo in annotation_dict:
                        annotation_dict[combo] = annotation_dict[combo] + 1
                    else:
                        annotation_dict[combo] = 1
        title = "N-gram"
        measure = 'Frequency'
        
        if stat_type == 'frequency':
            allAnnotations = float(allAnnotations)
            for pos, number in annotation_dict.items():
                try:
                    pos = pos.encode('utf8')
                    result_list.append((pos, number, "{0:.4f}".format(float(number)/allAnnotations)))
                except:
                    continue

            result_list = sorted(result_list, key=lambda x: x[1], reverse=True)
            if len(result_list) > 100:
                result_list = result_list[:100]
        else:
            allAnnotations = float(allAnnotations)
            for pos, number in annotation_dict.items():
                if stat_type == 'dis_legomena':
                    if number == 2:
                        pos = pos.encode('utf8')
                        result_list.append((pos, number, "{0:.4f}".format(float(number)/allAnnotations)))
                else:
                    if number == 1:
                        pos = pos.encode('utf8')
                        result_list.append((pos, number, "{0:.4f}".format(float(number)/allAnnotations)))
            if len(result_list) > 300:
                result_list = result_list[:300]
    else:
        all_annotations = []
        for doc in corpus:
            if doc.count('###') > 3:
                annotations = doc.split('###')
            else:
                annotations = doc.split()
            all_annotations.extend(annotations)

        if stat_type == 'pmi_bigrams':
            bigram_measures = nltk.collocations.BigramAssocMeasures()
            finder = BigramCollocationFinder.from_words(all_annotations)
            best = sorted(finder.score_ngrams(bigram_measures.pmi), key=lambda x: x[1], reverse=True)
            if len(best) > 100:
                best = best[:100]
            for tags, score in best:
                tag1, tag2 = tags
                result_list.append((tag1 + "\t" + tag2, "{0:.4f}".format(score)))
            title = "Bigram collocations"

        elif stat_type == 'pmi_trigrams':
            trigram_measures = nltk.collocations.TrigramAssocMeasures()
            finder = TrigramCollocationFinder.from_words(all_annotations)
            best = sorted(finder.score_ngrams(trigram_measures.pmi), key=lambda x: x[1], reverse=True)
            if len(best) > 100:
                best = best[:100]
            for tags, score in best:
                tag1, tag2, tag3 = tags
                result_list.append((tag1 + " " + tag2 + " " + tag3, "{0:.4f}".format(score)))
            title = "Trigram collocations"
        measure = 'PMI score'

    return render(request, 'visualizations/corpus_statistics.html', {'widget': widget, 'data': [result_list, title, measure, general_stats], 'narrow_doc': narrow_doc})


def corpus_to_csv(request,input_dict,output_dict,widget):
    from mothra.settings import MEDIA_ROOT
    from workflows.helpers import ensure_dir
    destination = MEDIA_ROOT+'/'+str(request.user.id)+'/'+str(widget.id)+'.csv'
    ensure_dir(destination)
    df = input_dict['df']
    df.to_csv(destination, encoding='utf-8', sep=',', index=False)
    filename = str(request.user.id)+'/'+str(widget.id)+'.csv'
    output_dict['filename'] = filename
    return render(request, 'visualizations/string_to_file.html',{'widget':widget,'input_dict':input_dict,'output_dict':output_dict})


def display_result(request,input_dict,output_dict,widget):
    score = input_dict['result']
    result = {'accuracy': str(score.mean()), 'std': str(score.std())}
    return render(request, 'visualizations/display_result.html',{'widget':widget,'result':result})

