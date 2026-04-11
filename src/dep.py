import pandas as pd



columns: list = ["wordnum", "form", "POS", "head", "deprel", "tokenid", "sentid"]
text_rows: list[list[str]] = []
sentid: int = 1
with open(treebank, encoding="utf8", errors="ignore") as f:
    for line in f:
        if line == "\n":
            sentid += 1
        else:
            contents: list[str] = line.strip().split()
            tokenid = contents[9].replace("TokenId=", "").replace(".word", "")
            if tokenid.count(".") == 2:
                tokenid = tokenid[:-2]
            selected_contents = [
                int(contents[0]),
                contents[1],
                contents[3],
                int(contents[6]),
                contents[7],
                tokenid,
                sentid,
            ]
            text_rows.append(selected_contents)
df = pd.DataFrame(text_rows, columns=columns)


df_non_punc = df[df["deprel"] != "punct"].reset_index(drop=True)

df_non_punc["dep_nc"] = 0
for index, row in df_non_punc.iterrows():
    count = len(
        df_non_punc[
            (df_non_punc["sentid"] == row["sentid"])
            & (df_non_punc["wordnum"] < row["wordnum"])
            & (df_non_punc["head"] == row["wordnum"])
        ].index
    )
    if row["head"] < row["wordnum"] and row["head"] != 0:
        count += 1
    df_non_punc.at[index, "dep_nc"] = count


df = pd.merge(
    df,
    df_non_punc,
    left_on=["wordnum", "form", "POS", "head", "deprel", "tokenid", "sentid"],
    right_on=["wordnum", "form", "POS", "head", "deprel", "tokenid", "sentid"],
    how="left",
)
df.fillna(0)
df = (
    df.groupby("tokenid")
    .agg(
        {
            "wordnum": lambda x: ",".join(map(str, x)),
            "form": lambda x: "".join(x),
            "POS": "first",
            "head": "first",
            "deprel": lambda x: ",".join(x),
            "sentid": "first",
            "dep_nc": "sum",
        }
    )
    .reset_index()
)
