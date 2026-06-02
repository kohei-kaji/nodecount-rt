import stanza

stanza.download("en")

nlp = stanza.Pipeline("en")

with open("./data/OneStop/sentences.txt", "r", encoding="utf-8") as f_in, open("./data/OneStop/stanza.txt", "w", encoding="utf-8") as f_out:
    for line in f_in:
        line = line.strip()
        doc = nlp(line)
        conll_str = "{:C}".format(doc)
        f_out.write(conll_str + "\n\n")
