# colex-db
Database of colexifications, with etymological information

## Steps

1. Fill in `NA`s in `wn_concepticon.tsv` (433 of them) to improve mapping between senses in NorthEuraLex and WordNet synsets. (Finish this before moving on to the others.)
2. Fill in `NA`s in `northeuralex_etymologies.tsv` to improve detection of cognate lexemes. (Huge task! Pick a subset of languages. Note, however, that the table *only* includes etymologies of polysemous words. Also note that it is only necessary to provide the etymology of a word if the etymon also exhibits the same polysemy. If it does not, then remove the row.)
3. Fill in `NA`s in `northeuralex_definitions.tsv` to improve detection of cognate colexifications. (Add rows as necessary, to allow for polysemies of proto-words.)
4. Check periodically to make sure that exactly the same words appear in `northeuralex_etymologies.tsv` and in `northeuralex_definitions.tsv`.

There will soon be a web interface for carrying out the above steps. In the meantime, just use Excel.

Once the web interface is ready, we will think about expanding beyond NorthEuraLex.
