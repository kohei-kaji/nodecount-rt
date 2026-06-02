# uv pip install "ccgtools[parser]@git+https://github.com/stanojevic/ccgtools"

import ccg
import os
os.environ["TOKENIZERS_PARALLELISM"] = "false"
ccgparser = ccg.parser("pretrained:English-RoBERTa")

with open("./data/OneStop/sentences.txt", "r") as infile, open("./data/OneStop/ccg.txt", "w") as outfile:
    for line in infile:
        text = line.strip()
        if not text:
            continue
        tree = ccgparser.parse_sent(text)
        print(f"{tree}", file=outfile)
