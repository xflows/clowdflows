#-*-coding:utf8-*-

import sys
import os

import cPickle as pickle
from StringIO import StringIO
import pycrfsuite
from sklearn.feature_extraction import DictVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn.pipeline import Pipeline
import re
import codecs
from sklearn.tree import DecisionTreeClassifier
import sys
import warnings
warnings.filterwarnings("ignore")

def tag_sent(sent, trie, tagger):
    return tagger.tag(extract_features_msd(sent, trie))


def tag_lemmatise_sent(sent, trie, tagger, lemmatiser):
    return [(a, get_lemma(b, a, lemmatiser)) for a, b in zip(tag_sent(sent, trie, tagger), sent)]


def get_lemma(token, msd, lemmatiser):
    lexicon = lemmatiser['lexicon']
    key = token.lower() + '_' + msd
    if key in lexicon:
        return lexicon[key][0].decode('utf8')
    if msd[:2] != 'Np':
        for i in range(len(msd) - 1):
            for key in lexicon.keys(key[:-(i + 1)]):
                return lexicon[key][0].decode('utf8')
    return guess_lemma(token, msd, lemmatiser)


def guess_lemma(token, msd, lemmatiser):
    if len(token) < 3:
        return apply_rule(token, "(0,'',0,'')", msd)
    model = lemmatiser['model']
    if msd not in model:
        return token
    else:
        lemma = apply_rule(token, model[msd].predict(
            extract_features_lemma(token))[0], msd)
        if len(lemma) > 0:
            return lemma
        else:
            return token


def suffix(token, n):
    if len(token) > n:
        return token[-n:]


def apply_rule(token, rule, msd):
    rule = list(eval(rule))
    if msd:
        if msd[:2] == 'Np':
            lemma = token
        else:
            lemma = token.lower()
    else:
        lemma = token.lower()
    rule[2] = len(token) - rule[2]
    lemma = rule[1] + lemma[rule[0]:rule[2]] + rule[3]
    return lemma


def tag_main(data):
    reload(sys)  # Reload does the trick!
    sys.setdefaultencoding('UTF8')
    docs, lang, lemmatize = data
    trie, tagger, lemmatiser = load_models(lang)
    tagged_docs=[]
    for tokens in docs:
        tagged_sents = []
        for s in tokens:
            sent = []
            for token in s:
              try:
                sent.append(token[0].decode('utf'))
              except:
                sent.append(' ')
            tag_counter = 0
            if not lemmatize:
                tags = tag_sent(sent, trie, tagger)

                tags_proper = []
                for token in sent:
                    if ' ' in token:
                        if len(token) == 1:
                            tags_proper.append(' ')
                        else:
                            tags_proper.append(token)
                    else:
                        tags_proper.append(tags[tag_counter])
                    tag_counter += 1
                tagged_sents.append(zip(sent, tags_proper))
            else:
                tags = tag_lemmatise_sent(sent, trie, tagger, lemmatiser)
                tags_proper = []
                for token in sent:
                    if ' ' in token:
                        if len(token) == 1:
                            tags_proper.append((' ',' ', ' '))
                        else:
                            tags_proper.append((token, token, token))
                    else:
                        tags_proper.append((token, tags[tag_counter][0], tags[tag_counter][1]))
                    tag_counter += 1
                tagged_sents.append(tags_proper)
        if not lemmatize:
            tagged_sents = " ".join([pos for sentence in tagged_sents for word, pos in sentence if word != ' '])
        else:
            tagged_sents = " ".join([lemma for sentence in tagged_sents for word, pos, lemma in sentence if word != ' '])
        tagged_docs.append(tagged_sents)
    return tagged_docs
        
def load_models(lang):
    reldir = os.path.join('workflows', 'nlp', 'models', 'reldi_tagger')
    trie = pickle.load(open(os.path.join(reldir, lang + '.marisa'), 'rb'))
    tagger = pycrfsuite.Tagger()
    tagger.open(os.path.join(reldir, lang + '.msd.model'))
    lemmatiser = {'model': pickle.load(open(os.path.join(reldir, lang + '.lexicon.guesser'), 'rb')),
                  'lexicon': pickle.load(open(os.path.join(reldir, lang + '.lexicon'), 'rb'))}
    return (trie, tagger, lemmatiser)
				  

#train lemmatizer				  
def suffix(token,n):
  if len(token)>n:
    return token[-n:]
	

def extract_features_lemma(token):
  features={}
  for i in range(4):
    suf=suffix(token,i+1)
    if suf!=None:
      features['suf'+str(i+1)]=suf
  if len(token)>3:
    features['pref3']=token[:3]
  return features
  
#train tagger
def extract_features_msd(sent,trie): #originally "combined2", relates to the model named "lexicon"
  full_sent=[]
  suffix_sent=[]
  for token in sent:
    full_sent.append(search_full(token.lower(),trie))
    #if full_sent[-1]==None: # uncomment for "msdsuf" feature
    #  suffix_sent.append(search_suffix(token.lower(),trie))
    #else:
    #  suffix_sent.append(None)
  features=[]
  for index,token in enumerate(sent):
    tfeat=[]
    tfeat.append('w[0]='+wpos(sent,index))
    tfeat.append('packed_shape='+packed_shape(token,index))
    for i in range(3): #w[-1] w[1]
      if wpos(sent,index-i-1)!=None:
        tfeat.append('w['+str(-i-1)+']='+wpos(sent,index-i-1))
      if wpos(sent,index+i+1)!=None:
        tfeat.append('w['+str(i+1)+']='+wpos(sent,index+i+1))
    for i in range(4): #w[0] suffix
      if wsuf(token,i+1)!=None:
        tfeat.append('s['+str(i+1)+']='+wsuf(token,i+1))
    if full_sent[index]!=None:
      for msd in full_sent[index]:
        tfeat.append('msd='+msd)
    #elif suffix_sent[index]!=None:
    #  for msd in suffix_sent[index]:
    #    tfeat.append('msdsuf='+msd)
    if full_sent[index]!=None:
      tfeat.append('inlexicon=True')
    else:
      tfeat.append('inlexicon=False')
    for i in range(2):
      if wpos(sent,index-i-1)!=None:
        msds=full_sent[index-i-1]
        if msds!=None:
          for msd in msds:
            tfeat.append('msd[-'+str(i+1)+']='+msd)#+':'+str(float(msds[msd])/sum(msds.values())))
      if wpos(sent,index+i+1)!=None:
        msds=full_sent[index+i+1]
        if msds!=None:
          for msd in msds:
            tfeat.append('msd['+str(i+1)+']='+msd)#+':'+str(float(msds[msd])/sum(msds.values())))
    if index==0:
      tfeat.append('__BOS__')
    elif index+1==len(sent):
      tfeat.append('__EOS__')
    features.append(tfeat)
  return features
   
def packed_shape(token,index):
  packed=''
  for char in token:
    if char.isupper():
      packed+='u'
    elif char.islower():
      packed+='l'
    elif char.isdigit():
      packed+='d'
    else:
      packed+='x'
  if index==0:
    packed+='_START'
  return re.sub(r'(.)\1{2,}',r'\1\1',packed)
  
def search_full(token,trie):
  token=reverse(u'_'+token)
  if token in trie:
    return [decode(e) for e in trie[token]]
  
def reverse(s):
  t=''
  for u in s:
    t=u+t
  return t
  
def decode(s):  
  return ''.join(s).strip('0')
  
def islcase(token):
  return token.lower()==token

def isnum(token):
  import re
  return re.search(r'\d',token)!=None

def transnum(token):
  import re
  return re.sub(r'\d','D',token)

def wpos(sent,index):
  if index>=0 and index<len(sent):
    return transnum(sent[index].lower())

def wsuf(token,length):
  if token==None:
    return
  if len(token)>length:
    token=transnum(token.lower())
    return token[-length:]

def getpos(tag):
  if tag not in gram_feat:
    return None
  return gram_feat[tag].get('pos')

def getgender(tag):
  if tag not in gram_feat:
    return None
  return gram_feat[tag].get('Gender')

def getnumber(tag):
  if tag not in gram_feat:
    return None
  return gram_feat[tag].get('Number')

def getcase(tag):
  if tag not in gram_feat:
    return None
  return gram_feat[tag].get('Case')
