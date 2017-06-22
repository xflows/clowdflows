import pandas as pd
from nltk import word_tokenize
from nltk import pos_tag
from nltk.corpus import stopwords
from sklearn.svm import LinearSVC, SVC
from sklearn import pipeline
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.pipeline import FeatureUnion
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer, TfidfTransformer
from sklearn.linear_model import LogisticRegression
from sklearn import model_selection
from sklearn.externals import joblib
from sklearn.metrics import accuracy_score
from sklearn import preprocessing
from sklearn.preprocessing import Normalizer
from itertools import groupby
import cPickle as pickle
import re
import os


#read different word lists and return a set of words
def read_wordList(file):
    with open(file, 'r') as f:
        return set([word.split('/')[0].strip().lower() for word in f if word])


def tokenize_n_character(text):
    return text.split()


def remove_punctuation(text):
    punctuation = '#@!"$%&()*+,-./:;<=>?[\]^_`{|}~' + "'"
    for p in punctuation:
        text = text.replace(p, "")
    return text


def remove_stopwords(text, lang):
    if lang == 'es':
        stops = set(stopwords.words("spanish"))
    elif lang == 'en':
        stops = set(stopwords.words("english"))
    elif lang == 'pt':
        stops = set(stopwords.words("portuguese"))
    else:
        return text
    text = text.split()
    text = [x.lower() for x in text if x.lower() not in stops]
    return " ".join(text)


def remove_mentions(text, replace_token):
    return re.sub(r'(?:@[\w_]+)', replace_token, text)


def remove_hashtags(text, replace_token):
    return re.sub(r"(?:\#+[\w_]+[\w\'_\-]*[\w_]+)", replace_token, text)


def remove_url(text, replace_token):
    regex = 'http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+'
    return re.sub(regex, replace_token, text)


def tag(tagger, text, sent_tokenize, lang):
    #for arab we dont do POS tagging
    if not sent_tokenize:
        return text
    else:
        #tokenize with nltk default tokenizer
        tokens = sent_tokenize.tokenize(text)
        #use average perceptron tagger
        tokens = [word_tokenize(token) for token in tokens]
        text = tagger.tag_sents(tokens)
        return " ".join(tag for sent in text for word, tag in sent)


def simplify_tag(t):
    if "+" in t:
        return t[t.index("+")+1:]
    else:
        return t


def get_emojis(path):
    emoji_dict = {}
    df_emojis = pd.read_csv(path, encoding="utf-8", delimiter=",")
    for index, row in df_emojis.iterrows():
        occurrences = row['Occurrences']
        pos = (row['Positive'] + 1) / (occurrences + 3)
        neg = (row['Negative'] + 1) / (occurrences + 3)
        sent = pos - neg
        emoji_dict[row['Emoji']] = sent
    return emoji_dict


def countCharacterFlooding(text):
    text = ''.join(text.split())
    groups = groupby(text)
    cnt = 0
    for label, group in groups:
        char_cnt = sum(1 for _ in group)
        if char_cnt > 2:
            cnt += 1
    return cnt


#count specific characters
def count_patterns(text, list):
    cnt=0
    length = len(text)
    for pattern in list:
        cnt += text.count(pattern)
    if length == 0:
        return 0
    return cnt/length

#get sentiment according to emojis
def get_sentiment(text, emoji_dict):
    sentiment = 0
    list = emoji_dict.keys()
    for pattern in list:
        text_cnt = text.count(pattern)
        sentiment += emoji_dict[pattern] * text_cnt
    return sentiment


def get_affix(text):
    return " ".join([word[-4:] if len(word) >= 4 else word for word in text.split()])


def affix_punct(text):
    punct = '!"$%&()*+,-./:;<=>?[\]^_`{|}~'
    ngrams = []
    for i, character in enumerate(text[0:-2]):
        ngram = text[i:i+3]
        if ngram[0]  in punct:
            for p in punct:
                if p in ngram[1:]:
                    break
            else:
                ngrams.append(ngram)
    return "###".join(ngrams)


def affix_punct_tokenize(text):
    tokens = text.split('###')
    return tokens


def get_ngrams(text):
    ngrams = []
    for word in text.split():
        if len(word) > 4:
            for i in range(len(word) - 3):
                ngrams.append(word[i:i + 4])
        else :
            ngrams.append(word)
    print(ngrams)
    return " ".join(ngrams)


#fit and transform text features, used in scikit Feature union
class text_col(BaseEstimator, TransformerMixin):
    def __init__(self, key):
        self.key = key
    def fit(self, x, y=None):
        return self
    def transform(self, data_dict):
        return data_dict[self.key]


#fit and transform numeric features, used in scikit Feature union
class digit_col(BaseEstimator, TransformerMixin):
    def fit(self, x, y=None):
        return self
    def transform(self, hd_searches):
        d_col_drops=['text', 'pos_tag', 'no_punctuation', 'no_stopwords', 'text_clean', 'affixes', 'affix_punct']
        hd_searches = hd_searches.drop(d_col_drops,axis=1).values
        scaler = preprocessing.MinMaxScaler().fit(hd_searches)
        return scaler.transform(hd_searches)


#preprocess and tag data and write it to csv for later use
def preprocess(df_data, lang, pos_tagger, sent_tokenizer):
    df_data['text_clean_r'] = df_data['text'].map(lambda x: remove_hashtags(x, '#HASHTAG'))
    df_data['text_clean_r'] = df_data['text_clean_r'].map(lambda x: remove_url(x, "HTTPURL"))
    df_data['text_clean_r'] = df_data['text_clean_r'].map(lambda x: remove_mentions(x, '@MENTION'))
    df_data['text_clean'] = df_data['text'].map(lambda x: remove_hashtags(x, ''))
    df_data['text_clean'] = df_data['text_clean'].map(lambda x: remove_url(x, ""))
    df_data['text_clean'] = df_data['text_clean'].map(lambda x: remove_mentions(x, ''))

    if lang != 'sl':
        df_data['pos_tag'] = df_data['text_clean'].map(lambda x: tag(pos_tagger, x, sent_tokenizer, lang))
    else:
        df_data['pos_tag'] = pd.Series(pos_tagger)

    df_data['no_punctuation'] = df_data['text_clean'].map(lambda x: remove_punctuation(x))
    df_data['no_stopwords'] = df_data['no_punctuation'].map(lambda x: remove_stopwords(x, lang))
    df_data['text_clean'] = df_data['text_clean_r']
    df_data = df_data.drop('text_clean_r', 1)
    return df_data


def createFeatures(df_data):
    emoji_path = os.path.join('workflows', 'nlp', 'models', 'emoji_dataset.csv')
    emoji_dict = get_emojis(emoji_path)
    emoji_list = emoji_dict.keys()

    df_data['affixes'] = df_data['text_clean'].map(lambda x: get_affix(x))
    df_data['affix_punct'] = df_data['text_clean'].map(lambda x: affix_punct(x))
    df_data['number_of_emojis'] = df_data['text_clean'].map(lambda x: count_patterns(x, emoji_list))
    df_data['sentiment'] = df_data['text_clean'].map(lambda x: get_sentiment(x, emoji_dict))
    df_data['number_of_character_floods'] = df_data['no_punctuation'].map(lambda x: countCharacterFlooding(x))
    return df_data

