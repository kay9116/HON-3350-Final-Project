import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
from collections import Counter

df = pd.read_csv("cleaned_transportation_survey.csv")

# ── Portrait theme ────────────────────────────────────────────────────────────

BG      = "#111111"
SCARLET = "#CC0000"
MUTED   = "#555555"
SOFT    = "#AAAAAA"
WHITE   = "#EEEEEE"
GOLD    = "#F6BE00"

plt.rcParams.update({
    "font.family":        ["Arial", "DejaVu Sans"],
    "figure.facecolor":   BG,
    "axes.facecolor":     BG,
    "text.color":         WHITE,
    "axes.labelcolor":    SOFT,
    "xtick.color":        SOFT,
    "ytick.color":        SOFT,
    "xtick.labelsize":    10,
    "ytick.labelsize":    11,
    "axes.spines.top":    False,
    "axes.spines.right":  False,
    "axes.spines.left":   False,
    "axes.spines.bottom": False,
    "figure.dpi":         150,
})

SOURCE = "Source: UH Undergraduate Student Life Survey, Spring 2026"


def save(name):
    plt.savefig(name, bbox_inches="tight", facecolor=BG)
    plt.close()
    print(f"Saved {name}")


# ── Chart 1: Residence ────────────────────────────────────────────────────────
# Bold number display — the numbers ARE the chart

q9  = df["Q9"].dropna()
on  = int((q9 == 1).sum())
off = int((q9 == 0).sum())
total = on + off

fig, ax = plt.subplots(figsize=(7, 4))
fig.subplots_adjust(left=0.08, right=0.92, top=0.78, bottom=0.22)
ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.axis("off")

# proportional split bar
bar_y, bar_h = 0.38, 0.18
on_w = on / total

ax.add_patch(plt.Rectangle((0, bar_y), on_w, bar_h, color=SCARLET, zorder=2))
ax.add_patch(plt.Rectangle((on_w, bar_y), 1 - on_w, bar_h, color=MUTED, zorder=2))

# big numbers
ax.text(on_w / 2,      bar_y + bar_h + 0.16, str(on),
        ha="center", va="bottom", fontsize=46, fontweight="bold", color=SCARLET)
ax.text(on_w + (1 - on_w) / 2, bar_y + bar_h + 0.16, str(off),
        ha="center", va="bottom", fontsize=46, fontweight="bold", color=SOFT)

# labels below bar
ax.text(on_w / 2,      bar_y - 0.06, "ON-CAMPUS",
        ha="center", va="top", fontsize=11, color=SCARLET, fontweight="bold")
ax.text(on_w + (1 - on_w) / 2, bar_y - 0.06, "OFF-CAMPUS",
        ha="center", va="top", fontsize=11, color=SOFT, fontweight="bold")

# percent labels inside bar
ax.text(on_w / 2,      bar_y + bar_h / 2, f"{on/total:.0%}",
        ha="center", va="center", fontsize=12, fontweight="bold", color=WHITE, zorder=3)
ax.text(on_w + (1 - on_w) / 2, bar_y + bar_h / 2, f"{off/total:.0%}",
        ha="center", va="center", fontsize=12, fontweight="bold", color=WHITE, zorder=3)

fig.text(0.5, 0.94, "Where Do UH Students Live?",
         ha="center", fontsize=17, fontweight="bold", color=WHITE)
fig.text(0.5, 0.06, f"n = {total}  |  {SOURCE}",
         ha="center", fontsize=8.5, color=SOFT, style="italic")

save("chart1_residence.png")


# ── Chart 2: Transport Mode — lollipop ───────────────────────────────────────

q10 = df["Q10"].dropna()
mode_counts = q10.value_counts().sort_values()

fig, ax = plt.subplots(figsize=(8, 5))
fig.subplots_adjust(left=0.22, right=0.88, top=0.85, bottom=0.2)

ys = range(len(mode_counts))
colors = [SCARLET if v == mode_counts.max() else MUTED for v in mode_counts.values]

ax.hlines(list(ys), 0, mode_counts.values,
          color=[MUTED]*len(ys), linewidth=1.5, zorder=1)
ax.scatter(mode_counts.values, list(ys),
           color=colors, s=180, zorder=2)

for i, (val, col) in enumerate(zip(mode_counts.values, colors)):
    ax.text(val + 0.2, i, str(val),
            va="center", fontsize=11, color=col, fontweight="bold")

ax.set_yticks(list(ys))
ax.set_yticklabels(mode_counts.index, fontsize=12, color=WHITE)
ax.set_xlim(0, mode_counts.max() + 2)
ax.xaxis.set_major_locator(ticker.MaxNLocator(integer=True))
ax.tick_params(axis="x", colors=SOFT, length=0)
ax.tick_params(axis="y", length=0)
ax.set_xlabel("Number of Students", fontsize=10, color=SOFT, labelpad=10)

ax.axvline(0, color=MUTED, linewidth=0.5)

fig.text(0.5, 0.93, "How UH Students Get to Campus",
         ha="center", fontsize=16, fontweight="bold", color=WHITE)
fig.text(0.5, 0.04,
         f"n = {q10.shape[0]}  |  {SOURCE}",
         ha="center", fontsize=8.5, color=SOFT, style="italic")

save("chart2_transport_mode.png")


# ── Chart 3: Commute time — dot histogram ─────────────────────────────────────
# Each dot = one student. Dots stack vertically within 20-min bins.

q11_off = df.loc[df["Q9"] == 0, "Q11"].dropna()

BIN = 20
binned   = [int(v // BIN) * BIN for v in q11_off]
bin_counts = Counter(binned)

xs, ys = [], []
for b, count in sorted(bin_counts.items()):
    cx = b + BIN / 2
    for i in range(count):
        xs.append(cx)
        ys.append(i)

fig, ax = plt.subplots(figsize=(10, 4.5))
fig.subplots_adjust(left=0.06, right=0.96, top=0.82, bottom=0.24)

ax.scatter(xs, ys, s=280, color=SCARLET, alpha=0.92,
           edgecolors=BG, linewidths=0.8, zorder=2)

mean_val = q11_off.mean()
ax.axvline(mean_val, color=GOLD, linewidth=1.5, linestyle="--", zorder=1)
ax.text(mean_val + 3, ax.get_ylim()[1] if len(ys) == 0 else max(ys) + 0.6,
        f"avg: {mean_val:.0f} min",
        fontsize=9, color=GOLD, va="bottom")

ax.set_xlabel("Total Daily Commute (minutes)", fontsize=10, color=SOFT, labelpad=10)
ax.set_xlim(-10, 260)
ax.set_ylim(-0.8, max(ys) + 1.5)
ax.set_yticks([])
ax.xaxis.set_major_locator(ticker.MultipleLocator(20))
ax.tick_params(axis="x", length=0)

# subtle vertical grid
for x in range(0, 261, 20):
    ax.axvline(x, color="#222222", linewidth=0.5, zorder=0)

fig.text(0.5, 0.93, "How Long Do Off-Campus Students Commute?",
         ha="center", fontsize=16, fontweight="bold", color=WHITE)
fig.text(0.5, 0.07,
         f"Each dot represents one student  |  n = {len(q11_off)}  |  {SOURCE}",
         ha="center", fontsize=8.5, color=SOFT, style="italic")

save("chart3_commute_time.png")


# ── Chart 4: Commute by mode — lollipop ──────────────────────────────────────

commute_by_mode = (
    df.dropna(subset=["Q10", "Q11"])
    .groupby("Q10")["Q11"]
    .agg(["mean", "count"])
    .rename(columns={"mean": "avg_commute", "count": "n"})
    .sort_values("avg_commute")
)

fig, ax = plt.subplots(figsize=(8, 5))
fig.subplots_adjust(left=0.22, right=0.88, top=0.85, bottom=0.2)

ys     = range(len(commute_by_mode))
colors = [SCARLET if v == commute_by_mode["avg_commute"].max()
          else MUTED for v in commute_by_mode["avg_commute"]]

ax.hlines(list(ys), 0, commute_by_mode["avg_commute"],
          color=[MUTED]*len(ys), linewidth=1.5, zorder=1)
ax.scatter(commute_by_mode["avg_commute"], list(ys),
           color=colors, s=180, zorder=2)

for i, (col, (idx, row)) in enumerate(zip(colors, commute_by_mode.iterrows())):
    ax.text(row["avg_commute"] + 2, i,
            f"{row['avg_commute']:.0f} min  (n={int(row['n'])})",
            va="center", fontsize=10, color=col, fontweight="bold")

ax.set_yticks(list(ys))
ax.set_yticklabels(commute_by_mode.index, fontsize=12, color=WHITE)
ax.set_xlim(0, commute_by_mode["avg_commute"].max() + 60)
ax.tick_params(axis="x", colors=SOFT, length=0)
ax.tick_params(axis="y", length=0)
ax.set_xlabel("Average Daily Commute (minutes)", fontsize=10, color=SOFT, labelpad=10)
ax.axvline(0, color=MUTED, linewidth=0.5)

fig.text(0.5, 0.93, "Transit Riders Face the Longest Commutes",
         ha="center", fontsize=16, fontweight="bold", color=WHITE)
fig.text(0.5, 0.04,
         f"Average round-trip commute by primary mode  |  {SOURCE}",
         ha="center", fontsize=8.5, color=SOFT, style="italic")

save("chart4_commute_by_mode.png")


print("\nAll 4 charts saved.")
