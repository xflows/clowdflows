#!/usr/bin/python
#-*-coding:utf8-*-

import sys
import os
import cPickle as pickle
from random import randint

tm_lambda=0.2
lm_lambda=0.8

def get_uppers(token_list):
  uppers=[]
  for token in token_list:
    uppers.append([])
    for index,char in enumerate(token):
      if char.isupper():
        uppers[-1].append(index)
  return uppers

def apply_uppers(uppers,token_list):
  for token_index,indices in enumerate(uppers):
    token=token_list[token_index]
    for index in indices:
      if index<len(token):
        token=token[:index]+token[index].upper()+token[index+1:]
    token_list[token_index]=token
  return token_list

def redi(token_list,lexicon,lm=None):
  uppers=get_uppers(token_list)
  token_list=[e.lower() for e in token_list]
  indices=[]
  for index,token in enumerate(token_list):
    if token in lexicon:
      if len(lexicon[token])==1:
        token_list[index]=lexicon[token].keys()[0]
      else:
        if lm==None:
          token_list[index]=sorted(lexicon[token].items(),key=lambda x:-x[1])[0][0]
        else:
          indices.append(index)
  for index in indices:
    hypotheses={}
    for hypothesis in lexicon[token_list[index]]:
      sent=' '.join(token_list[:index]+[hypothesis]+token_list[index+1:])
      hypotheses[hypothesis]=lm_lambda*lm.score(sent)+tm_lambda*lexicon[token_list[index]][hypothesis]
    token_list[index]=sorted(hypotheses,key=lambda x:-hypotheses[x])[0]
  return apply_uppers(uppers,token_list)

def restore_diacritic(tokens, lexicon):
  result = []
  for sent in tokens:
      token_list = redi([e[0].decode('utf8').encode('utf8') for e in sent], lexicon)
      position_list = [(start, finish) for tok, start, finish in sent]
      sent = [(tok, pos[0], pos[1]) for tok, pos in zip(token_list, position_list)]
      result.append(sent)
  return result


