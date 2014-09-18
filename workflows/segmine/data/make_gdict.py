import pickle
from urllib import urlopen
import csv


def get_genes(g2ont_fname):
    genes = []
    fp = urlopen(g2ont_fname)
    #with open(g2ont_fname, 'r') as fp:
    for line in fp:
        line = line.strip()
        if not line:
            continue

        line = eval(line)
        genes.append(line[0].lower())
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


def probe2rep_STU():
    d = {}
    with open('STU_probe2rep.csv') as fp:
        reader = csv.reader(fp)
        for row in reader:
            probe = str(row[0])
            gmm = str(row[2]).lower()
            d[probe] = gmm
    with open('probe2rep_STU.pickle', 'w') as fp:
        pickle.dump(d, fp, pickle.HIGHEST_PROTOCOL)
#end


if __name__ == "__main__":
    build_dicts()
    probe2rep_STU()

