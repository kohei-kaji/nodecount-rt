This repository contains the code and data for the paper titled "The Dual Nature of Syntactic Node Count: Facilitating and Inhibiting Sentence Comprehension".

## Calculating predictors
Node Count values can be computed using the scripts in `./src/`.
We obtained silver-standard parses using the [Berkeley Neural Parser](https://github.com/nikitakit/self-attentive-parser) (`benepar_en3`, `en_core_web_md`), [Stanza](https://stanfordnlp.github.io/stanza/depparse.html), and [ccgtools](https://github.com/stanojevic/ccgtools).

Unigram and conditional surprisals were estimated using [llm_surprisal](https://github.com/byungdoh/llm_surprisal/tree/eacl24) and [wt_decoding](https://github.com/byungdoh/wt_decoding/tree/main).


## Reading-time analyses
The scripts for the reading time analyses are located in `./analysis/`.

## Visualization
All figures reported in the paper can be reproduced using the scripts in `./visualize/`.
