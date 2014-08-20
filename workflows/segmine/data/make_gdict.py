import pickle
from urllib import urlopen


def get_genes(g2ont_fname):
    genes = []
    fp = urlopen(g2ont_fname)
    #with open(g2ont_fname, 'r') as fp:
    for line in fp:
        line=line.strip()
        if not line:
            continue

        line = eval(line)
        genes.append(line[0])
    return dict.fromkeys(genes)
#end


def build_dicts():
    stu = get_genes('http://www.gomapman.org/export/current/segs/stu2gomapman')
    with open('genes_stu.pickle', 'w') as fp:
        pickle.dump(stu, fp, pickle.HIGHEST_PROTOCOL)

    ath = get_genes('http://www.gomapman.org/export/current/segs/ath2gomapman')
    with open('genes_ath.pickle', 'w') as fp:
        pickle.dump(ath, fp, pickle.HIGHEST_PROTOCOL)
#end


#build_dicts()

