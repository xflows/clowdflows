from triplet_extractor import data_structures as ds
from triplet_extractor import tripletExtraction as te
from os.path import normpath, join, dirname

dname = normpath(dirname(__file__))

doc = ds.Document()
doc.loadString(open(join(dname, 'triplet_extractor/vocabulary/pmc2556844.txt')).read())
ds.SentenceSplitter().splitNLTK(doc)
gtc = ds.GeniaTTC()
gtc.process(doc)
voc = te.Vocabulary()
voc.loadCompounds_file(join(dname, 'triplet_extractor/vocabulary/compounds.lst'))
voc.loadPredicates_files(activationFname=join(dname, 'triplet_extractor/vocabulary/activation.lst'),
                         activations_rotate=join(dname, 'triplet_extractor/vocabulary/activation_rotate.lst'),
                         inhibitionFname=join(dname, 'triplet_extractor/vocabulary/inhibition.lst'),
                         bindingFname=join(dname, 'triplet_extractor/vocabulary/binding.lst'),
                         activationFname_passive=join(dname, 'triplet_extractor/vocabulary/activation_pas.lst'),
                         inhibitionFname_passive=join(dname, 'triplet_extractor/vocabulary/inhibition_pas.lst'),
                         bindingFname_passive=join(dname, 'triplet_extractor/vocabulary/binding_pas.lst'))


ex = te.TripletExtractor(voc)
triplets = ex.extractTripletsNLP(doc, VP_CHECK_POS=1)
print 'Triplets found: ', len(triplets)
