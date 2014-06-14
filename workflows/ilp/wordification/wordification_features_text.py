__author__ = 'matic'
from collections import defaultdict

from wordification import Wordification

from services.webservice import WebService

def get_cross_validation_accuracy(arff):
    acs=[]
    for a in range(10):
        j48 = WebService("http://vihar.ijs.si:8092/Classification?wsdl")
        j48_response = j48.client.J48(params="")
        j48_learner = j48_response['J48_learner']

        arff2weka = WebService("http://vihar.ijs.si:8092/Utilities?wsdl")
        arff2weka_response = arff2weka.client.arff_to_weka_instances(arff=arff,class_index="")

        instances = arff2weka_response['instances']

        cv = WebService("http://vihar.ijs.si:8092/Evaluation?wsdl",timeout=600)
        cv_response = cv.client.cross_validate(learner=j48_learner,instances=instances,folds=5)

        accuracy = cv_response['accuracy']

        acs.append(float(accuracy))

    return sum(acs)*1./len(acs)


def split_string_to_words(string):
    documents=string.split("\n")
    words=set([])
    for document in documents:
        #print document.split(" ")
        words |= set(document.split(" "))
    #print words
    return words

def prunning_name(perc):
    if perc:
        return "Prunning "+str(perc)+"%"
    else:
        return "No prunning"

class Wordification_features_test(object):

    def __init__(self,target_table,other_tables,context):
        self.max_witem_length=6
        self.results=[]
        self.feature_counts=[]
        self.accuracies=[]

        for prunning_percentage in [None,20,40]:
            rez_a=[]
            rez_c=[]
            for word_att_length in range(1,self.max_witem_length):
                print "percentage:",prunning_percentage,"witem:",word_att_length
                pruned=Wordification(target_table,other_tables,context,word_att_length)
                pruned.run()
                pruned.calculate_tf_idfs("tfidf")
                if prunning_percentage:
                    pruned.prune(prunning_percentage)

                wordification_string=pruned.wordify()

                rez_c.append(len(split_string_to_words(wordification_string)))
                a= pruned.to_arff()
                rez_a.append(get_cross_validation_accuracy(a))
            self.feature_counts.append([prunning_percentage,rez_c])
            self.accuracies.append([prunning_percentage,rez_a])

    # import os
    # os.environ.setdefault("DJANGO_SETTINGS_MODULE", "mothra.settings")
    # from services.webservice import WebService
    #
    # def weka_stuff_todo(self):
    #     input_dict={}
    #     f = input_dict['file']
    #     fname = os.path.basename(input_dict['file'])
    #     wsdl = input_dict.get('wsdl', 'http://vihar.ijs.si:8092/Evaluation?wsdl')
    #     ws = WebService(wsdl, 60000)
    #     response = ws.client.cross_validate(fileName=fname, inFile=data)

    def print_results(self):
        import matplotlib.pyplot as plt

        #len(self.nonpruned_feature_count)
        #x = np.linspace(1, len(self.results))
        #print self.nonpruned_feature_count
        #print self.pruned_feature_count

        for prunning_percentage,feature_counts in self.feature_counts:
            plt.plot(range(1,len(feature_counts)+1), feature_counts, '-',marker='o', linewidth=2,label=prunning_name(prunning_percentage))
        #plt.plot(range(1,len(self.pruned_feature_count)+1), self.pruned_feature_count, '-',marker='o', linewidth=2)

        #plt.plot(x, y, 'k')
        #plt.title('Damped exponential decay')#, fontdict=font)
        #plt.text(2, 0.65, r'$\cos(2 \pi t) \exp(-t)$')#, fontdict=font)
        plt.xlabel('Number of witems per word')#, fontdict=font)
        plt.ylabel('Number of generated features')#, fontdict=font)
        plt.legend(title="Prunning percentage", loc="best")

        plt.grid()
        #plt.show()
        plt.savefig('featuresWitems.jpg')
        plt.clf()

        for prunning_percentage,accuracies in self.accuracies:
            plt.plot(range(1,len(accuracies)+1), accuracies, '-',marker='o', linewidth=2,label=prunning_name(prunning_percentage))

        plt.xlabel('Number of witems per word')#, fontdict=font)
        plt.ylabel('Classification accuracy')#, fontdict=font)
        plt.legend(title="Prunning percentage", loc="best")
        plt.grid()
        plt.savefig('accuraciesWitems.jpg')
        plt.clf()
        print self.feature_counts
        print self.accuracies


