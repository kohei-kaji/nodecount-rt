import pandas as pd
import matplotlib.pyplot as plt

file_names = ["sweep_lambda_unk5.csv", "sweep_lambda_unk10.csv"]

plt.figure(figsize=(10, 6))

for file_name in file_names:
    df = pd.read_csv(file_name)
    label_name = file_name.split("_")[-1].replace(".csv", "")
    plt.plot(df["lam"], df["weighted_pearson_r"], marker="o", linestyle="-", label=f"unk={label_name[3:]}")

plt.xlabel("Lambda")
plt.ylabel("Weighted Pearson r")
plt.legend(title="Unknown Rate")
plt.grid(True, linestyle="--", alpha=0.7)
plt.savefig("lambda_vs_weighted_pearson.png")


plt.figure(figsize=(10, 6))

for file_name in file_names:
    df = pd.read_csv(file_name)
    label_name = file_name.split("_")[-1].replace(".csv", "")
    plt.errorbar(df["lam"], df["mean_pmi"], yerr=df["sd_pmi"], marker="o", linestyle="-", capsize=5, label=f"unk={label_name[3:]}")

plt.xlabel("Lambda")
plt.ylabel("Mean PMI")
plt.legend(title="Unknown Rate")
plt.grid(True, linestyle="--", alpha=0.7)

plt.savefig("lambda_vs_mean_pmi.png")
