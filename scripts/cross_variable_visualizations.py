# Author: Vincent Wren
# All chart concepts, design choices, and analytical decisions were conceived by myself.
# Claude AI (Anthropic) was used as a technical assistant to help implement and debug the code.
# No ideation, interpretation, or analytical judgment was performed by AI.

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from pathlib import Path

ROOT = Path(__file__).parent.parent
df = pd.read_csv(ROOT / "data/cleaned/combined_cleaned.csv")

# ── Portrait theme ────────────────────────────────────────────────────────────

BG      = "#111111"
SCARLET = "#CC0000"
MUTED   = "#444444"
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
    "axes.spines.top":    False,
    "axes.spines.right":  False,
    "axes.spines.left":   False,
    "axes.spines.bottom": False,
    "figure.dpi":         150,
})

SOURCE = "Source: UH Undergraduate Student Life Survey, Spring 2026"

def save(name):
    plt.savefig(ROOT / "charts" / name, bbox_inches="tight", facecolor=BG)
    plt.close()
    print(f"Saved charts/{name}")


# ── Chart 5: Class standing vs. Residence ────────────────────────────────────
# Lower class (1st–2nd yr) vs Upper class (3rd–4th yr), split by on/off campus

df["class_group"] = df["Q2"].map({1: "Lower Class\n(1st–2nd yr)",
                                   2: "Lower Class\n(1st–2nd yr)",
                                   3: "Upper Class\n(3rd–4th yr)",
                                   4: "Upper Class\n(3rd–4th yr)"})

groups     = [("Lower Class", "1st–2nd year"),
              ("Upper Class", "3rd–4th year")]
group_keys = ["Lower Class\n(1st–2nd yr)", "Upper Class\n(3rd–4th yr)"]
group_data = df.dropna(subset=["class_group", "Q9"])

fig, ax = plt.subplots(figsize=(8, 4.5))
fig.subplots_adjust(left=0.05, right=0.95, top=0.80, bottom=0.18)
ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.axis("off")

bar_h = 0.16
bar_w = 0.38
gap   = 0.10
x_starts = [(1 - 2 * bar_w - gap) / 2,
            (1 - 2 * bar_w - gap) / 2 + bar_w + gap]
bar_y = 0.30

for i, ((title, subtitle), key) in enumerate(zip(groups, group_keys)):
    subset  = group_data[group_data["class_group"] == key]
    n_total = len(subset)
    n_on    = int((subset["Q9"] == 1).sum())
    n_off   = int((subset["Q9"] == 0).sum())
    on_w    = (n_on / n_total) * bar_w
    off_w   = (n_off / n_total) * bar_w
    x       = x_starts[i]
    cx      = x + bar_w / 2

    # Group title (bold) + subtitle (year range)
    ax.text(cx, 0.72, title, ha="center", va="center",
            fontsize=14, fontweight="bold", color=WHITE)
    ax.text(cx, 0.65, subtitle, ha="center", va="center",
            fontsize=10, color=SOFT, style="italic")

    # Bar segments
    ax.add_patch(plt.Rectangle((x, bar_y), on_w, bar_h, color=SCARLET, zorder=2))
    ax.add_patch(plt.Rectangle((x + on_w, bar_y), off_w, bar_h, color=MUTED, zorder=2))

    # Percentages inside bars
    if n_on > 0:
        ax.text(x + on_w / 2, bar_y + bar_h / 2, f"{n_on/n_total:.0%}",
                ha="center", va="center", fontsize=10, fontweight="bold",
                color=WHITE, zorder=3)
    if n_off > 0:
        ax.text(x + on_w + off_w / 2, bar_y + bar_h / 2, f"{n_off/n_total:.0%}",
                ha="center", va="center", fontsize=10, fontweight="bold",
                color=WHITE, zorder=3)

    # Counts below bar (labeled)
    ax.text(x + on_w / 2, bar_y - 0.04,
            f"{n_on} on-campus", ha="center", va="top",
            fontsize=8.5, color=SCARLET, fontweight="bold")
    ax.text(x + on_w + off_w / 2, bar_y - 0.04,
            f"{n_off} off-campus", ha="center", va="top",
            fontsize=8.5, color=SOFT, fontweight="bold")

    # Total students under each group
    ax.text(cx, bar_y - 0.16,
            f"n = {n_total} students", ha="center", va="top",
            fontsize=9, color=SOFT, style="italic")

fig.text(0.5, 0.92, "Does Class Standing Predict Where Students Live?",
         ha="center", fontsize=15, fontweight="bold", color=WHITE)
fig.text(0.5, 0.06,
         f"Total respondents: {len(group_data)}  |  {SOURCE}",
         ha="center", fontsize=8.5, color=SOFT, style="italic")

save("chart5_class_vs_residence.png")


# ── Chart 6: On/Off Campus vs. Voted On-Campus ───────────────────────────────
# Did students vote on-campus in the March Primary? Split by residence.

vote_data = df.dropna(subset=["Q9", "Q4"]).copy()

# Build 3-way voting category from Q4 + Q5
OFFCAMPUS_INDICATORS = ("I voted at a another polling location",
                        "I voted during Early Voting")

def vote_category(row):
    if row["Q4"] == 1:
        return "voted_on"
    q5 = str(row["Q5"]) if pd.notna(row["Q5"]) else ""
    if any(ind in q5 for ind in OFFCAMPUS_INDICATORS):
        return "voted_off"
    return "did_not"

vote_data["vote_cat"] = vote_data.apply(vote_category, axis=1)

# Color palette — both voting categories share the red family,
# non-voters are a clearly separate gray.
COL_VOTED_ON  = SCARLET     # bright red
COL_VOTED_OFF = "#7A1A1A"   # darker red (still in voted family)
COL_NOT_VOTED = "#666666"   # gray — visually distinct from voters
BRACKET_COL   = "#E55555"   # lighter red for the "voted" bracket

groups = [("On-Campus",  "lives on-campus", 1),
          ("Off-Campus", "lives off-campus", 0)]

fig, ax = plt.subplots(figsize=(9, 5))
fig.subplots_adjust(left=0.05, right=0.95, top=0.78, bottom=0.22)
ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.axis("off")

bar_h = 0.16
bar_w = 0.40
gap   = 0.08
x_starts = [(1 - 2 * bar_w - gap) / 2,
            (1 - 2 * bar_w - gap) / 2 + bar_w + gap]
bar_y = 0.34

for i, (title, subtitle, res_val) in enumerate(groups):
    subset    = vote_data[vote_data["Q9"] == res_val]
    n_total   = len(subset)
    n_on      = int((subset["vote_cat"] == "voted_on").sum())
    n_off     = int((subset["vote_cat"] == "voted_off").sum())
    n_not     = int((subset["vote_cat"] == "did_not").sum())

    on_w  = (n_on  / n_total) * bar_w
    off_w = (n_off / n_total) * bar_w
    not_w = (n_not / n_total) * bar_w
    x     = x_starts[i]
    cx    = x + bar_w / 2

    # Group title — both gray
    ax.text(cx, 0.74, title, ha="center", va="center",
            fontsize=14, fontweight="bold", color=SOFT)
    ax.text(cx, 0.67, subtitle, ha="center", va="center",
            fontsize=10, color=SOFT, style="italic")

    # 3-segment bar
    ax.add_patch(plt.Rectangle((x, bar_y), on_w, bar_h,
                                color=COL_VOTED_ON, zorder=2))
    ax.add_patch(plt.Rectangle((x + on_w, bar_y), off_w, bar_h,
                                color=COL_VOTED_OFF, zorder=2))
    ax.add_patch(plt.Rectangle((x + on_w + off_w, bar_y), not_w, bar_h,
                                color=COL_NOT_VOTED, zorder=2))

    # "Voted" bracket spanning the two voting segments
    voted_w = on_w + off_w
    if voted_w > 0:
        bx0 = x
        bx1 = x + voted_w
        by_top  = bar_y + bar_h + 0.045
        by_drop = bar_y + bar_h + 0.005
        # horizontal line
        ax.plot([bx0, bx1], [by_top, by_top],
                color=BRACKET_COL, linewidth=1.5, zorder=4)
        # vertical drops at each end
        ax.plot([bx0, bx0], [by_top, by_drop],
                color=BRACKET_COL, linewidth=1.5, zorder=4)
        ax.plot([bx1, bx1], [by_top, by_drop],
                color=BRACKET_COL, linewidth=1.5, zorder=4)
        # "voted" label above bracket
        n_voted = n_on + n_off
        ax.text((bx0 + bx1) / 2, by_top + 0.015,
                f"voted ({n_voted}, {n_voted/n_total:.0%})",
                ha="center", va="bottom", fontsize=9.5,
                color=BRACKET_COL, fontweight="bold", zorder=4)

    # Percentages inside each segment (only if enough room)
    def seg_text(seg_x, seg_w, count, txt_col):
        if count == 0 or seg_w < 0.025:
            return
        ax.text(seg_x + seg_w / 2, bar_y + bar_h / 2,
                f"{count/n_total:.0%}", ha="center", va="center",
                fontsize=10, fontweight="bold", color=txt_col, zorder=3)

    seg_text(x,                 on_w,  n_on,  WHITE)
    seg_text(x + on_w,          off_w, n_off, WHITE)
    seg_text(x + on_w + off_w,  not_w, n_not, SOFT)

    # Counts below bar — three short labels
    ax.text(x + on_w / 2,                 bar_y - 0.04,
            f"{n_on}", ha="center", va="top",
            fontsize=10, color=COL_VOTED_ON, fontweight="bold")
    ax.text(x + on_w + off_w / 2,         bar_y - 0.04,
            f"{n_off}", ha="center", va="top",
            fontsize=10, color=COL_VOTED_OFF, fontweight="bold")
    ax.text(x + on_w + off_w + not_w / 2, bar_y - 0.04,
            f"{n_not}", ha="center", va="top",
            fontsize=10, color=SOFT, fontweight="bold")

    # Total students under each group
    ax.text(cx, bar_y - 0.13,
            f"n = {n_total} students", ha="center", va="top",
            fontsize=9, color=SOFT, style="italic")

# Color key at bottom
key_y = 0.10
keys  = [(COL_VOTED_ON,  "Voted on-campus"),
         (COL_VOTED_OFF, "Voted off-campus"),
         (COL_NOT_VOTED, "Did not vote")]
key_w = 0.20
key_gap = 0.02
total_key_w = 3 * key_w + 2 * key_gap
key_x = (1 - total_key_w) / 2
for col, label in keys:
    ax.add_patch(plt.Rectangle((key_x, key_y), 0.018, 0.022,
                                color=col, zorder=2))
    ax.text(key_x + 0.025, key_y + 0.011, label,
            ha="left", va="center", fontsize=9, color=SOFT)
    key_x += key_w + key_gap

fig.text(0.5, 0.92, "Do On-Campus Students Vote On-Campus More?",
         ha="center", fontsize=15, fontweight="bold", color=WHITE)
fig.text(0.5, 0.04,
         f"March 2026 Primary  |  Total: {len(vote_data)}  |  {SOURCE}",
         ha="center", fontsize=8.5, color=SOFT, style="italic")

save("chart6_voted_vs_residence.png")


print("\nAll cross-variable charts saved.")
