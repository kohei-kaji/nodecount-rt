import pandas as pd

df = pd.read_csv("ia_Paragraph_ordinary.csv")
df = df[df["article_id"] != 0].copy()

def get_canonical_text(series):
    return series.mode().iloc[0]

df_canonical_paras = df.groupby(["article_batch", "article_id", "difficulty_level", "paragraph_id"])["paragraph"].agg(get_canonical_text).reset_index()
df_canonical_paras = df_canonical_paras.sort_values(by=["article_batch", "article_id", "difficulty_level", "paragraph_id"])
df_articles = df_canonical_paras.groupby(["article_batch", "article_id", "difficulty_level"])[("paragraph")].apply(lambda x: ' '.join(x)).reset_index()

df_articles.rename(columns={"paragraph": "article"}, inplace=True)

df_articles.to_csv("articles.csv", index=False)

ia_counts = df.groupby(["article_batch", "article_id", "difficulty_level", "paragraph_id", "IA_ID"])[("IA_LABEL")].nunique()
buggy_ias = ia_counts[ia_counts > 1].reset_index()
buggy_keys = buggy_ias[["article_batch", "article_id", "difficulty_level", "paragraph_id", "IA_ID"]]

df_clean = df.merge(buggy_keys, on=["article_batch", "article_id", "difficulty_level", "paragraph_id", "IA_ID"], how="outer", indicator=True)

retained = ["article_batch", "article_id", "difficulty_level", "paragraph_id", "IA_ID", "IA_LABEL", "participant_id", "is_correct", "IA_SKIP", "IA_REGRESSION_IN", "IA_REGRESSION_OUT", "IA_FIRST_FIXATION_DURATION", "IA_FIRST_RUN_DWELL_TIME", "IA_REGRESSION_PATH_DURATION", "IA_DWELL_TIME", "universal_pos", "ptb_pos"]

df_clean = df_clean[df_clean["_merge"] == "left_only"].drop(columns=["_merge"])
df_clean = df_clean[retained].copy()

unique_ias = df_clean[["article_batch", "article_id", "difficulty_level", "paragraph_id", "IA_ID"]].drop_duplicates()

unique_ias = unique_ias.sort_values(by=["article_batch", "article_id", "difficulty_level", "paragraph_id", "IA_ID"])

unique_ias["zone"] = unique_ias.groupby(["article_batch", "article_id", "difficulty_level"]).cumcount() + 1

df_clean = df_clean.merge(unique_ias, on=["article_batch", "article_id", "difficulty_level", "paragraph_id", "IA_ID"], how="left")

df_clean = df_clean.sort_values(by=["article_batch", "article_id", "difficulty_level", "participant_id", "zone"])
df_clean.to_csv("rts.csv", index=False)
