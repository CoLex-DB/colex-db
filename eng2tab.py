#!/usr/share/python
# -*- encoding: utf-8 -*-
#
# Extract synset-word pairs from the Princeton Wordnet

import nltk, codecs
from nltk.corpus import wordnet as w

#
# header
#
outfile = "wn-data-eng.tab"
o = codecs.open(outfile, "w", "utf-8" )
log = codecs.open("log",  "w", "utf-8")

o.write("WordNet_ID\tSynset\n")

for s in w.all_synsets():
    synset = "%08d-%s" % (s.offset(), s.pos())
    if synset.endswith('s'):
        synset = synset[:-1] + 'a'
        log.write('Satellite changed to:\t%s\n' % synset)
    elif synset.endswith('a'):
        log.write('Focal adjective:\t%s\n' % synset)
    o.write("%s\t%s\n" %  (synset, s.name()))
