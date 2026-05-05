import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np

df = pd.read_csv("cleaned_transportation_survey.csv")

# ── Shared style ──────────────────────────────────────────────────────────────

SCARLET  = "#CC0000"
GRAY     = "#6B6B6B"
LIGHT    = "#F2F2F2"
DARK     = "#1C1C1C"
GOLD     = "#F6BE00"

plt.rcParams.update({
    "font.family":    "sans-serif",
    "font.size":      11,
    "axes.spines.top":    False,
    "axes.spines.right":  False,
    "figure.dpi":     150,
})

SOURCE = "Source: UH Undergraduate Student Life Survey, Spring 2026"


# ── Chart 1: On-Campus vs. Off-Campus Residence (Q9) ─────────────────────────

q9  = df["Q9"].dropna()
on  = int((q9 == 1).sum())
off = int((q9 == 0).sum())

fig, ax = plt.subplots(figsize=(6, 7))
fig.subplots_adjust(bottom=0.18)

wedges, texts, autotexts = ax.pie(
    [on, off],
    labels=None,
    autopct="%1.0f%%",
    startangle=90,
    colors=[SCARLET, GRAY],
    wedgeprops={"width": 0.5, "edgecolor": "white", "linewidth": 2},
    pctdistance=0.75,
)
for at in autotexts:
    at.set_fontsize(14)
    at.set_fontweight("bold")
    at.set_color("white")

ax.set_title("Where Do UH Students Live?",
             fontsize=16, fontweight="bold", pad=20, color=DARK)

legend = [
    mpatches.Patch(color=SCARLET, label=f"On-Campus  (n={on})"),
    mpatches.Patch(color=GRAY,    label=f"Off-Campus  (n={off})"),
]
ax.legend(handles=legend, loc="lower center", bbox_to_anchor=(0.5, -0.08),
          frameon=False, fontsize=12)

fig.text(0.5, 0.04, f"n = {on + off}  |  {SOURCE}",
         ha="center", fontsize=9, color=GRAY, style="italic")

plt.savefig("chart1_residence.png", bbox_inches="tight")
plt.close()
print("Saved chart1_residence.png")


# ── Chart 2: Primary Mode of Transportation (Q10) ────────────────────────────

q10 = df["Q10"].dropna()
mode_counts = q10.value_counts().sort_values()

fig, ax = plt.subplots(figsize=(8, 5))
fig.subplots_adjust(bottom=0.18)

bars = ax.barh(
    mode_counts.index,
    mode_counts.values,
    color=[SCARLET if v == mode_counts.max() else GRAY for v in mode_counts.values],
    edgecolor="white",
    height=0.6,
)
for bar, val in zip(bars, mode_counts.values):
    ax.text(val + 0.15, bar.get_y() + bar.get_height() / 2,
            str(val), va="center", fontsize=11, color=DARK)

ax.set_xlabel("Number of Respondents", fontsize=11, color=GRAY)
ax.set_xlim(0, mode_counts.max() + 2)
ax.tick_params(axis="y", labelsize=12)
ax.set_title("How UH Students Get to Campus",
             fontsize=16, fontweight="bold", pad=14, color=DARK)

fig.text(0.5, 0.04,
         f"Personal vehicles are the dominant mode of transit  |  n = {q10.shape[0]}  |  {SOURCE}",
         ha="center", fontsize=9, color=GRAY, style="italic")

plt.savefig("chart2_transport_mode.png", bbox_inches="tight")
plt.close()
print("Saved chart2_transport_mode.png")


# ── Chart 3: Commute Time Distribution (Q11) — Off-Campus Students Only ───────
# On-campus students did not report commute times (not applicable), so only
# off-campus responses are shown here.

q11_off = df.loc[df["Q9"] == 0, "Q11"].dropna()

fig, ax = plt.subplots(figsize=(9, 5))
fig.subplots_adjust(bottom=0.18)

bins = range(0, 270, 20)

ax.hist(q11_off, bins=bins, color=SCARLET, alpha=0.85,
        edgecolor="white")

ax.axvline(q11_off.mean(), color=DARK, linestyle="--", linewidth=1.8,
           label=f"Average commute: {q11_off.mean():.0f} min")

ax.set_xlabel("Total Daily Commute Time (minutes)", fontsize=11, color=GRAY)
ax.set_ylabel("Number of Students", fontsize=11, color=GRAY)
ax.set_title("How Long Does It Take Off-Campus Students to Commute?",
             fontsize=14, fontweight="bold", pad=14, color=DARK)
ax.legend(frameon=False, fontsize=10)

fig.text(0.5, 0.04,
         f"Off-campus respondents only  |  n = {len(q11_off)}  |  {SOURCE}",
         ha="center", fontsize=9, color=GRAY, style="italic")

plt.savefig("chart3_commute_time.png", bbox_inches="tight")
plt.close()
print("Saved chart3_commute_time.png")


# ── Chart 4: Average Commute Time by Transportation Mode (Q10 vs Q11) ─────────

commute_by_mode = (
    df.dropna(subset=["Q10", "Q11"])
    .groupby("Q10")["Q11"]
    .agg(["mean", "count"])
    .rename(columns={"mean": "avg_commute", "count": "n"})
    .sort_values("avg_commute")
)

fig, ax = plt.subplots(figsize=(9, 5))
fig.subplots_adjust(bottom=0.18)

bars = ax.barh(
    commute_by_mode.index,
    commute_by_mode["avg_commute"],
    color=[SCARLET if v == commute_by_mode["avg_commute"].max() else GRAY
           for v in commute_by_mode["avg_commute"]],
    edgecolor="white",
    height=0.6,
)
for bar, (idx, row) in zip(bars, commute_by_mode.iterrows()):
    ax.text(bar.get_width() + 1,
            bar.get_y() + bar.get_height() / 2,
            f"{row['avg_commute']:.0f} min  (n={int(row['n'])})",
            va="center", fontsize=10, color=DARK)

ax.set_xlabel("Average Daily Commute (minutes)", fontsize=11, color=GRAY)
ax.set_xlim(0, commute_by_mode["avg_commute"].max() + 55)
ax.tick_params(axis="y", labelsize=12)
ax.set_title("Transit Riders Face the Longest Commutes",
             fontsize=15, fontweight="bold", pad=14, color=DARK)

fig.text(0.5, 0.04,
         f"Average round-trip commute by primary mode of transportation  |  {SOURCE}",
         ha="center", fontsize=9, color=GRAY, style="italic")

plt.savefig("chart4_commute_by_mode.png", bbox_inches="tight")
plt.close()
print("Saved chart4_commute_by_mode.png")


print("\nAll 4 charts saved.")
