# Author: Vincent Wren
# All chart concepts, design choices, and analytical decisions were conceived by myself.
# Claude AI (Anthropic) was used as a technical assistant to help implement and debug the code.
# No ideation, interpretation, or analytical judgment was performed by AI.
#
# Regenerates the 5 cross-variable statistical charts (transportation × civic
# engagement) with a consistent layout: no overlapping labels, group-level
# n values rendered above the chart footer, and a standard footer line with
# the total n and source.

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from pathlib import Path
from scipy import stats

ROOT = Path(__file__).parent.parent
df = pd.read_csv(ROOT / "data/cleaned/combined_cleaned.csv")

# ── Style ─────────────────────────────────────────────────────────────────────

BG          = "#D9D4C7"     # darker tan so white legend boxes have contrast
INK         = "#1A1A1A"
SOFT        = "#555555"
GRID        = "#B8B2A3"
SOURCE_TEXT = "Source: UH Undergraduate Student Life Survey, Spring 2026"

plt.rcParams.update({
    "font.family":        ["Arial", "DejaVu Sans"],
    "figure.facecolor":   BG,
    "axes.facecolor":     BG,
    "text.color":         INK,
    "axes.labelcolor":    INK,
    "xtick.color":        INK,
    "ytick.color":        INK,
    "axes.edgecolor":     SOFT,
    "axes.spines.top":    False,
    "axes.spines.right":  False,
    "axes.grid":          True,
    "grid.color":         GRID,
    "grid.linestyle":     ":",
    "grid.linewidth":     0.6,
    "figure.dpi":         150,
})

LEGEND_ELEMS = [
    Line2D([0], [0], marker="D", color="none", markerfacecolor="white",
           markeredgecolor=INK, markersize=8, label="Mean"),
    Line2D([0], [0], color="white", linewidth=2, label="Median"),
]

MODE_COLORS = {
    "METRO Rail":   "#4F8FC2",
    "Carpool":      "#5BAE8B",
    "Personal Car": "#D6A24B",
    "METRO Bus":    "#C46B5C",
    "Walking":      "#5BA386",
    "Bicycle":      "#9B6BB0",
}


def footer(fig, n, extra=None):
    """Standard footer used on every chart."""
    parts = [f"n = {n}", SOURCE_TEXT]
    if extra:
        parts.insert(1, extra)
    fig.text(0.5, 0.02, "  |  ".join(parts),
             ha="center", fontsize=8.5, color=SOFT, style="italic")


def title_block(fig, subtitle):
    fig.text(0.5, 0.945, subtitle, ha="center",
             fontsize=14, color=INK, fontweight="bold")


def style_boxes(bp, edge_color, fill_color):
    for patch in bp["boxes"]:
        patch.set_facecolor(fill_color)
        patch.set_edgecolor(edge_color)
        patch.set_linewidth(1.5)
    for med in bp["medians"]:
        med.set_color("white"); med.set_linewidth(2.2)
    for w in bp["whiskers"]: w.set_color(edge_color); w.set_linewidth(1.2)
    for c in bp["caps"]:     c.set_color(edge_color); c.set_linewidth(1.2)
    for f in bp["fliers"]:   f.set_marker("")


def save(name):
    plt.savefig(ROOT / "charts" / name, bbox_inches="tight", facecolor=BG)
    plt.close()
    print(f"Saved charts/{name}")


def add_two_group_pvalue(ax, p, y_frac=0.92, label_prefix="p"):
    """Bracket between the two boxes (positions 1 and 2)."""
    y_lim = ax.get_ylim()
    y     = y_lim[0] + (y_lim[1] - y_lim[0]) * y_frac
    drop  = (y_lim[1] - y_lim[0]) * 0.015
    ax.plot([1, 1, 2, 2], [y - drop, y, y, y - drop],
            color=SOFT, linewidth=1)
    star = " *" if p < 0.05 else ""
    ax.text(1.5, y + drop * 0.6, f"{label_prefix} = {p:.3f}{star}",
            ha="center", va="bottom", fontsize=9, color=SOFT)


def add_group_n_labels(ax, labels, counts):
    """Place 'n = X' below each x-tick label, well clear of the axis."""
    ax.set_xticks(range(1, len(labels) + 1))
    ax.set_xticklabels(labels, fontsize=10)
    for i, (lab, n) in enumerate(zip(labels, counts), start=1):
        ax.annotate(f"n = {n}",
                    xy=(i, 0), xytext=(0, -28),
                    xycoords=("data", "axes fraction"),
                    textcoords="offset points",
                    ha="center", va="top",
                    fontsize=8.5, color=SOFT, style="italic")


# ── Chart A: Civic Engagement by Major (bar) ─────────────────────────────────

major_stats = (
    df.dropna(subset=["Q3", "Q8_index"])
      .groupby("Q3")["Q8_index"]
      .agg(["mean", "std", "count"])
      .sort_values("mean", ascending=True)
)
n_total = int(major_stats["count"].sum())

fig, ax = plt.subplots(figsize=(10, 6.2))
fig.subplots_adjust(top=0.86, bottom=0.14, left=0.30, right=0.96)

colors = ["#5BAE8B" if m > 0 else "#C46B5C" for m in major_stats["mean"]]
y_pos  = np.arange(len(major_stats))
ax.barh(y_pos, major_stats["mean"], color=colors,
        edgecolor=INK, linewidth=0.6, height=0.65, zorder=3)

# Error bars (±1 SD) only where SD is defined (n > 1)
errs = major_stats["std"].fillna(0).values
mask = major_stats["count"].values > 1
ax.errorbar(major_stats["mean"][mask], y_pos[mask],
            xerr=errs[mask], fmt="none",
            ecolor=SOFT, elinewidth=1, capsize=4, zorder=4)

# n= labels past the END of the right-side error bar — always right-aligned
xpad = 0.10
for i, (mean, sd, n) in enumerate(zip(major_stats["mean"],
                                       major_stats["std"].fillna(0),
                                       major_stats["count"])):
    xt = mean + abs(sd) + xpad
    ax.text(xt, i, f"n = {int(n)}", va="center", ha="left",
            fontsize=8.5, color=SOFT)

ax.set_yticks(y_pos)
ax.set_yticklabels([m.replace("Buisness", "Business") for m in major_stats.index],
                   fontsize=10)
ax.axvline(0, color=INK, linewidth=0.8)
ax.set_xlabel("Mean Civic Engagement Score (Q8 index)",
              fontsize=10, color=INK, labelpad=10)
# Extend xlim to fit error bars + n-labels
right_edge = (major_stats["mean"] + major_stats["std"].fillna(0)).max() + 0.7
left_edge  = (major_stats["mean"] - major_stats["std"].fillna(0)).min() - 0.3
ax.set_xlim(left_edge, right_edge)
ax.grid(axis="x")
ax.set_axisbelow(True)

ax.legend(handles=[Patch(facecolor="#5BAE8B", edgecolor=INK, label="Above average"),
                   Patch(facecolor="#C46B5C", edgecolor=INK, label="Below average")],
          loc="lower right", frameon=True, fontsize=9,
          facecolor=BG, edgecolor=GRID)

title_block(fig, "Civic Engagement Score by Major")
footer(fig, n_total)
save("barplot_major_vs_civic_engagement.png")


# ── Helper for 2-group boxplots ──────────────────────────────────────────────

def two_group_boxplot(label_a, label_b, data_a, data_b,
                      color_a, color_b, subtitle, fname,
                      ylabel="Civic Engagement Score (Q8 index)"):
    fig, ax = plt.subplots(figsize=(9, 6))
    fig.subplots_adjust(top=0.86, bottom=0.18, left=0.10, right=0.95)

    bp = ax.boxplot([data_a, data_b], positions=[1, 2],
                    widths=0.55, patch_artist=True,
                    showmeans=True,
                    meanprops=dict(marker="D", markerfacecolor="white",
                                   markeredgecolor=INK, markersize=8))
    # Style each box with its own color
    bp["boxes"][0].set_facecolor(color_a); bp["boxes"][0].set_edgecolor(color_a)
    bp["boxes"][0].set_alpha(0.45);        bp["boxes"][0].set_linewidth(1.5)
    bp["boxes"][1].set_facecolor(color_b); bp["boxes"][1].set_edgecolor(color_b)
    bp["boxes"][1].set_alpha(0.45);        bp["boxes"][1].set_linewidth(1.5)
    for j, c in enumerate([color_a, color_b]):
        bp["medians"][j].set_color("white"); bp["medians"][j].set_linewidth(2.2)
        for w in bp["whiskers"][j*2:(j+1)*2]: w.set_color(c); w.set_linewidth(1.2)
        for cap in bp["caps"][j*2:(j+1)*2]:   cap.set_color(c); cap.set_linewidth(1.2)
    for f in bp["fliers"]: f.set_marker("")

    # Jittered points
    rng = np.random.default_rng(0)
    for pos, data, color in [(1, data_a, color_a), (2, data_b, color_b)]:
        jitter = rng.uniform(-0.10, 0.10, size=len(data))
        ax.scatter(pos + jitter, data, color=color, s=28,
                   alpha=0.7, edgecolor=color, linewidth=0.8, zorder=4)

    add_group_n_labels(ax, [label_a, label_b], [len(data_a), len(data_b)])
    ax.set_ylabel(ylabel, fontsize=10, color=INK, labelpad=10)
    ax.set_xlim(0.4, 2.6)
    pad_y = (max(max(data_a), max(data_b)) - min(min(data_a), min(data_b))) * 0.20
    ax.set_ylim(min(min(data_a), min(data_b)) - pad_y * 0.6,
                max(max(data_a), max(data_b)) + pad_y * 1.4)

    _, p = stats.ttest_ind(data_a, data_b, equal_var=False)
    add_two_group_pvalue(ax, p, y_frac=0.92)

    ax.legend(handles=LEGEND_ELEMS, loc="upper right", frameon=True,
              fontsize=9, facecolor=BG, edgecolor=GRID)

    title_block(fig, subtitle)
    footer(fig, len(data_a) + len(data_b))
    save(fname)


# ── Chart B: Residence ──────────────────────────────────────────────────────
res = df.dropna(subset=["Q9", "Q8_index"])
on   = res.loc[res["Q9"] == 1, "Q8_index"].values
off_ = res.loc[res["Q9"] == 0, "Q8_index"].values
two_group_boxplot("On-Campus", "Off-Campus", on, off_,
                  "#4F8FC2", "#D6A24B",
                  "Civic Engagement by Residence",
                  "boxplot_residence_vs_civic_engagement.png")


# ── Chart C: Vehicle Ownership ──────────────────────────────────────────────
veh = df.dropna(subset=["Q13", "Q8_index"])
owns = veh.loc[veh["Q13"] == 1, "Q8_index"].values
none = veh.loc[veh["Q13"] == 0, "Q8_index"].values
two_group_boxplot("Owns Vehicle", "No Vehicle", owns, none,
                  "#D6A24B", "#4F8FC2",
                  "Civic Engagement Score by Vehicle Ownership",
                  "boxplot_vehicle_vs_civic_engagement.png")


# ── Chart D: Voting Behavior (index validation) ─────────────────────────────
vote = df.dropna(subset=["Q4", "Q8_index"])
voted    = vote.loc[vote["Q4"] == 1, "Q8_index"].values
notvoted = vote.loc[vote["Q4"] == 0, "Q8_index"].values

fig, ax = plt.subplots(figsize=(9, 6))
fig.subplots_adjust(top=0.86, bottom=0.18, left=0.10, right=0.95)

bp = ax.boxplot([voted, notvoted], positions=[1, 2], widths=0.55,
                patch_artist=True, showmeans=True,
                meanprops=dict(marker="D", markerfacecolor="white",
                               markeredgecolor=INK, markersize=8))
voted_col, nv_col = "#5BAE8B", "#C46B5C"
for j, c in enumerate([voted_col, nv_col]):
    bp["boxes"][j].set_facecolor(c); bp["boxes"][j].set_edgecolor(c)
    bp["boxes"][j].set_alpha(0.45);  bp["boxes"][j].set_linewidth(1.5)
    bp["medians"][j].set_color("white"); bp["medians"][j].set_linewidth(2.2)
    for w in bp["whiskers"][j*2:(j+1)*2]: w.set_color(c); w.set_linewidth(1.2)
    for cap in bp["caps"][j*2:(j+1)*2]:   cap.set_color(c); cap.set_linewidth(1.2)
for f in bp["fliers"]: f.set_marker("")

rng = np.random.default_rng(0)
for pos, data, color in [(1, voted, voted_col), (2, notvoted, nv_col)]:
    jitter = rng.uniform(-0.10, 0.10, size=len(data))
    ax.scatter(pos + jitter, data, color=color, s=28,
               alpha=0.7, edgecolor=color, linewidth=0.8, zorder=4)

add_group_n_labels(ax, ["Voted", "Did Not Vote"], [len(voted), len(notvoted)])
ax.set_ylabel("Civic Engagement Score (Q8 index)",
              fontsize=10, color=INK, labelpad=10)
ax.set_xlim(0.4, 2.6)
pad_y = (max(max(voted), max(notvoted)) - min(min(voted), min(notvoted))) * 0.20
ax.set_ylim(min(min(voted), min(notvoted)) - pad_y * 0.6,
            max(max(voted), max(notvoted)) + pad_y * 1.4)

_, p = stats.ttest_ind(voted, notvoted, equal_var=False)
add_two_group_pvalue(ax, p, y_frac=0.92)

ax.legend(handles=LEGEND_ELEMS, loc="upper right", frameon=True,
          fontsize=9, facecolor=BG, edgecolor=GRID)

title_block(fig, "Civic Engagement Score by Voting Behavior (Q4)")
footer(fig, len(voted) + len(notvoted))
save("boxplot_voting_vs_civic_engagement.png")


# ── Chart E-pre: Year in college vs civic engagement (ANOVA) ────────────────

year_df = df.dropna(subset=["Q2", "Q8_index"]).copy()
year_df["Q2"] = year_df["Q2"].astype(int)
year_order  = sorted(year_df["Q2"].unique())
year_labels = [{1: "1st year", 2: "2nd year",
                3: "3rd year", 4: "4th year+"}[y] for y in year_order]
year_groups = [year_df.loc[year_df["Q2"] == y, "Q8_index"].values
               for y in year_order]
year_counts = [len(g) for g in year_groups]
n_total     = sum(year_counts)

fig, ax = plt.subplots(figsize=(10, 6))
fig.subplots_adjust(top=0.86, bottom=0.20, left=0.10, right=0.95)

YEAR_COLORS = ["#9B6BB0", "#4F8FC2", "#5BAE8B", "#D6A24B"]
positions = np.arange(1, len(year_order) + 1)
bp = ax.boxplot(year_groups, positions=positions, widths=0.55,
                patch_artist=True, showmeans=True,
                meanprops=dict(marker="D", markerfacecolor="white",
                               markeredgecolor=INK, markersize=8))

for j, c in enumerate(YEAR_COLORS[:len(year_order)]):
    bp["boxes"][j].set_facecolor(c); bp["boxes"][j].set_edgecolor(c)
    bp["boxes"][j].set_alpha(0.45);  bp["boxes"][j].set_linewidth(1.5)
    bp["medians"][j].set_color("white"); bp["medians"][j].set_linewidth(2.2)
    for w in bp["whiskers"][j*2:(j+1)*2]: w.set_color(c); w.set_linewidth(1.2)
    for cap in bp["caps"][j*2:(j+1)*2]:   cap.set_color(c); cap.set_linewidth(1.2)
for f in bp["fliers"]: f.set_marker("")

rng = np.random.default_rng(0)
for pos, data, c in zip(positions, year_groups, YEAR_COLORS):
    jitter = rng.uniform(-0.10, 0.10, size=len(data))
    ax.scatter(pos + jitter, data, color=c, s=28, alpha=0.7,
               edgecolor=c, linewidth=0.8, zorder=4)

add_group_n_labels(ax, year_labels, year_counts)
ax.set_ylabel("Civic Engagement Score (Q8 index)",
              fontsize=10, color=INK, labelpad=10)
ax.set_xlim(0.4, len(year_order) + 0.6)
all_vals = np.concatenate(year_groups)
pad_y    = (all_vals.max() - all_vals.min()) * 0.18
ax.set_ylim(all_vals.min() - pad_y * 0.6, all_vals.max() + pad_y * 1.4)

# Pearson r against numeric year (treat year as ordinal)
x_year = year_df["Q2"].values
y_eng  = year_df["Q8_index"].values
r, p   = stats.pearsonr(x_year, y_eng)
star   = " *" if p < 0.05 else ""
text   = f"Pearson r = {r:.2f}\np = {p:.3f}{star}\nn = {n_total}"
ax.text(0.98, 0.96, text, transform=ax.transAxes,
        ha="right", va="top", fontsize=9, color=INK,
        bbox=dict(boxstyle="round,pad=0.4", facecolor=BG,
                  edgecolor=GRID, linewidth=0.8))

ax.legend(handles=LEGEND_ELEMS, loc="upper left", frameon=True,
          fontsize=9, facecolor=BG, edgecolor=GRID)

title_block(fig, "Civic Engagement Score by Year in College")
footer(fig, n_total)
save("boxplot_year_vs_civic_engagement.png")


# ── Chart E: Transport Mode (ANOVA) ─────────────────────────────────────────
mode_df = df.dropna(subset=["Q10", "Q11", "Q8_index"]).copy()
# Order modes by mean engagement, highest first
mode_order = (mode_df.groupby("Q10")["Q8_index"].mean()
                     .sort_values(ascending=False).index.tolist())
mode_groups = [mode_df.loc[mode_df["Q10"] == m, "Q8_index"].values
               for m in mode_order]
mode_counts = [len(g) for g in mode_groups]
n_total     = sum(mode_counts)

fig, ax = plt.subplots(figsize=(11, 6))
fig.subplots_adjust(top=0.86, bottom=0.20, left=0.08, right=0.96)

positions = np.arange(1, len(mode_order) + 1)
bp = ax.boxplot(mode_groups, positions=positions, widths=0.55,
                patch_artist=True, showmeans=True,
                meanprops=dict(marker="D", markerfacecolor="white",
                               markeredgecolor=INK, markersize=8))

for j, mode in enumerate(mode_order):
    c = MODE_COLORS.get(mode, "#888888")
    bp["boxes"][j].set_facecolor(c); bp["boxes"][j].set_edgecolor(c)
    bp["boxes"][j].set_alpha(0.45);  bp["boxes"][j].set_linewidth(1.5)
    bp["medians"][j].set_color("white"); bp["medians"][j].set_linewidth(2.2)
    for w in bp["whiskers"][j*2:(j+1)*2]: w.set_color(c); w.set_linewidth(1.2)
    for cap in bp["caps"][j*2:(j+1)*2]:   cap.set_color(c); cap.set_linewidth(1.2)
for f in bp["fliers"]: f.set_marker("")

rng = np.random.default_rng(0)
for pos, data, mode in zip(positions, mode_groups, mode_order):
    c = MODE_COLORS.get(mode, "#888888")
    jitter = rng.uniform(-0.10, 0.10, size=len(data))
    ax.scatter(pos + jitter, data, color=c, s=28, alpha=0.7,
               edgecolor=c, linewidth=0.8, zorder=4)

add_group_n_labels(ax, mode_order, mode_counts)
ax.set_ylabel("Civic Engagement Score (Q8 index)",
              fontsize=10, color=INK, labelpad=10)
ax.set_xlim(0.4, len(mode_order) + 0.6)

all_vals = np.concatenate(mode_groups)
pad_y = (all_vals.max() - all_vals.min()) * 0.18
ax.set_ylim(all_vals.min() - pad_y * 0.6, all_vals.max() + pad_y * 1.4)

# ANOVA stat box
groups_with_var = [g for g in mode_groups if len(g) > 1]
if len(groups_with_var) >= 2:
    F, p = stats.f_oneway(*groups_with_var)
    text = f"One-way ANOVA\nF = {F:.2f}, p = {p:.3f}\nn = {n_total}"
    ax.text(0.98, 0.96, text, transform=ax.transAxes,
            ha="right", va="top", fontsize=9, color=INK,
            bbox=dict(boxstyle="round,pad=0.4", facecolor=BG,
                      edgecolor=GRID, linewidth=0.8))

ax.legend(handles=LEGEND_ELEMS, loc="upper left", frameon=True,
          fontsize=9, facecolor=BG, edgecolor=GRID)

title_block(fig, "Civic Engagement Score by Transport Mode")
footer(fig, n_total)
save("boxplot_mode_vs_civic_engagement.png")


# ── Chart F: Scatter — Commute Time vs. Civic Engagement ─────────────────────

scatter_df = df.dropna(subset=["Q11", "Q8_index", "Q10"])
n_total    = len(scatter_df)

fig, ax = plt.subplots(figsize=(11, 6.5))
fig.subplots_adjust(top=0.88, bottom=0.16, left=0.10, right=0.95)

# Points per transport mode
for mode in sorted(scatter_df["Q10"].unique()):
    sub = scatter_df[scatter_df["Q10"] == mode]
    ax.scatter(sub["Q11"], sub["Q8_index"],
               s=70, color=MODE_COLORS.get(mode, "#888888"),
               edgecolor=INK, linewidth=0.6, alpha=0.85,
               label=mode, zorder=4)

# Regression line and Pearson r
x = scatter_df["Q11"].values
y = scatter_df["Q8_index"].values
slope, intercept = np.polyfit(x, y, 1)
xs = np.array([x.min() - 5, x.max() + 5])
ax.plot(xs, slope * xs + intercept,
        color=SOFT, linestyle="--", linewidth=1.5, zorder=3)

r, p = stats.pearsonr(x, y)

# Stats box (top-right)
star = " *" if p < 0.05 else ""
text = f"r = {r:.2f}\np = {p:.3f}{star}\nn = {n_total}"
ax.text(0.98, 0.96, text, transform=ax.transAxes,
        ha="right", va="top", fontsize=9.5, color=INK,
        bbox=dict(boxstyle="round,pad=0.4", facecolor=BG,
                  edgecolor=GRID, linewidth=0.8))

ax.set_xlabel("Commute Time (minutes)", fontsize=10, color=INK, labelpad=10)
ax.set_ylabel("Civic Engagement Score (Q8 index)",
              fontsize=10, color=INK, labelpad=10)
ax.set_xlim(x.min() - 10, x.max() + 15)
y_pad = (y.max() - y.min()) * 0.10
ax.set_ylim(y.min() - y_pad, y.max() + y_pad)
ax.grid(True)
ax.set_axisbelow(True)

ax.legend(title="Transport Mode", loc="lower right",
          frameon=True, fontsize=9, facecolor=BG, edgecolor=GRID,
          title_fontsize=9.5)

title_block(fig, "Commute Length vs. Civic Engagement Score")
footer(fig, n_total)
save("scatter_commute_vs_civic_engagement.png")


print("\nAll 7 cross-variable statistical charts regenerated.")
