# pip install "ccgtools[parser]@git+https://github.com/stanojevic/ccgtools"

import ccg
import os
os.environ["TOKENIZERS_PARALLELISM"] = "false"
ccgparser = ccg.parser("pretrained:English-RoBERTa")
tree = ccgparser.parse_sent("You can parse this sentence with this tool.")
print(tree)
