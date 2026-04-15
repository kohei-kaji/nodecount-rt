import spacy

# import benepar
# benepar.download("benepar_en3")


def cons_parse(input_path: str, output_path: str) -> None:
    nlp = spacy.load('en_core_web_md')
    nlp.add_pipe("benepar", config={"model": "benepar_en3"})


    with open(input_path, "r") as infile, open(output_path, "w") as outfile:
        for doc in infile:
            parses = nlp(doc)
            for sent in list(parses.sents):
                print(f"{sent._.parse_string}", file=outfile)
