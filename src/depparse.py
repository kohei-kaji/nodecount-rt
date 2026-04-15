import stanza

stanza.download("en")

nlp = stanza.Pipeline("en")

doc = nlp("The quick brown fox jumps over the lazy dog.")

for sentence in doc.sentences:
    sentence.print_dependencies()
