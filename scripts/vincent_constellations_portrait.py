"""
Constellations of Civic Life — a creative data portrait.

Encodes 5 independent dimensions from the merged undergraduate survey:
    X (horizontal)    = commute time in minutes (Q11). On-campus students live
                        in a separate strip at the left edge.
    Y (vertical)      = civic engagement score (Q8 PCA index). Y = 0 sits in
                        the middle of the sky; negative scores fall below,
                        positive scores rise above.
    Color             = major (Q3)
    Size              = year in college (Q2; 1–4)
    Brightness + spikes = voting behavior (Q4). Voters shine with diffraction
                        spikes and a hot core; non-voters appear dim. Y shows
                        self-reported engagement, brightness shows civic
                        action — they are different things.

Output: charts/portrait_constellations.png
"""

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# ─── Paths ──────────────────────────────────────────────────────────────
ROOT = Path(__file__).resolve().parents[1]
DATA_PATH = ROOT / "data" / "cleaned" / "combined_cleaned.csv"
OUT_PATH = ROOT / "charts" / "portrait_constellations.png"

# ─── Load data ──────────────────────────────────────────────────────────
df = pd.read_csv(DATA_PATH)
df = df[df["Q8_index"].notna()].reset_index(drop=True)

# Major color palette — 10 distinct hues, well-separated on a dark navy sky
PALETTE = {
    "Engineering":                       "#FFCD3C",  # gold
    "Liberal Arts and Social Sciences":  "#6FB6FF",  # sky blue
    "Art, Music, Dance, Theater":        "#FF8AB5",  # pink
    "Natural Sciences and Mathematics":  "#4ED98D",  # emerald
    "Buisness":                          "#FF9540",  # orange  (kept misspelled as in source)
    "Public Policy":                     "#BC8FFF",  # lavender
    "Architecture":                      "#3DCFD6",  # teal
    "Nursing":                           "#FF6868",  # light red
    "Integrated Studies":                "#FFCBA8",  # peach
    "Education":                         "#E84DD3",  # magenta
}
MAJOR_ORDER = df["Q3"].value_counts().index.tolist()

# Pretty-print "Buisness" → "Business" in legend without touching source data
def display_major(name: str) -> str:
    return name.replace("Buisness", "Business")

# Year → star core size (1st years small, 4th years dramatically larger)
YEAR_SIZE = {1: 60, 2: 150, 3: 270, 4: 410}

# ─── Geometry / sky regions ────────────────────────────────────────────
# Y range: engagement score, centered on 0 in the visual middle of the sky
Y_RANGE = 6.0  # total vertical span the sky uses
S_MIN = df["Q8_index"].min()
S_MAX = df["Q8_index"].max()
S_BOUND = max(abs(S_MIN), abs(S_MAX)) * 1.1  # symmetric padding around 0

def y_from_score(score: float) -> float:
    """Map an engagement score to a Y coordinate, centered on 0."""
    return (score / S_BOUND) * (Y_RANGE / 2)

# X range:
#   x ∈ [0.6, 1.8]  — "on campus" strip (no commute)
#   x ∈ [2.6, 12.0] — main sky, mapped 0..240 commute-minutes
ON_CAMPUS_X_CENTER = 1.2
ON_CAMPUS_X_WIDTH = 0.9
SKY_X_MIN, SKY_X_MAX = 2.6, 12.0
COMMUTE_MAX = 240  # minutes

def x_from_commute(commute: float) -> float:
    """Map an off-campus student's commute (minutes) to an X coordinate."""
    return SKY_X_MIN + (commute / COMMUTE_MAX) * (SKY_X_MAX - SKY_X_MIN)

# ─── Build star positions ──────────────────────────────────────────────
rng = np.random.default_rng(19)

records = []
for sid, row in df.iterrows():
    on_campus = bool(row["Q9"] == 1)
    if on_campus or pd.isna(row["Q11"]):
        x = ON_CAMPUS_X_CENTER + rng.uniform(-ON_CAMPUS_X_WIDTH / 2, ON_CAMPUS_X_WIDTH / 2)
    else:
        x = x_from_commute(row["Q11"]) + rng.uniform(-0.10, 0.10)
    y = y_from_score(row["Q8_index"]) + rng.uniform(-0.06, 0.06)
    records.append(
        dict(
            x=x,
            y=y,
            major=row["Q3"],
            year=int(row["Q2"]),
            score=row["Q8_index"],
            commute=row["Q11"],
            on_campus=on_campus,
            voted=row["Q4"],
        )
    )
pdf = pd.DataFrame(records)


def resolve_overlaps(pdf: pd.DataFrame, min_dist: float = 0.32, iters: int = 80) -> pd.DataFrame:
    """Push stars apart with a few rounds of pairwise repulsion."""
    pdf = pdf.copy()
    for _ in range(iters):
        moved = False
        for i in pdf.index:
            for j in pdf.index:
                if i >= j:
                    continue
                dx = pdf.at[i, "x"] - pdf.at[j, "x"]
                dy = pdf.at[i, "y"] - pdf.at[j, "y"]
                d = (dx * dx + dy * dy) ** 0.5
                if d < min_dist and d > 0.001:
                    push = (min_dist - d) / 2
                    pdf.at[i, "x"] += dx / d * push
                    pdf.at[j, "x"] -= dx / d * push
                    pdf.at[i, "y"] += dy / d * push * 0.6
                    pdf.at[j, "y"] -= dy / d * push * 0.6
                    moved = True
        if not moved:
            break
    return pdf

pdf = resolve_overlaps(pdf)

# Keep on-campus stars inside the on-campus strip after overlap resolution
in_strip = pdf["on_campus"]
pdf.loc[in_strip, "x"] = pdf.loc[in_strip, "x"].clip(
    ON_CAMPUS_X_CENTER - ON_CAMPUS_X_WIDTH / 2,
    ON_CAMPUS_X_CENTER + ON_CAMPUS_X_WIDTH / 2,
)

# ─── Brightness & diffraction-spike helpers ────────────────────────────
SCORE_LO, SCORE_HI = pdf["score"].min(), pdf["score"].max()

# Brightness encodes VOTING (Q4) — independent of Y (engagement score).
#   voted (Q4 == 1) → bright, with diffraction spikes and a hot white core
#   did not vote    → dim, no spikes
#   missing         → muted middle treatment
def brightness(voted) -> float:
    if pd.isna(voted):
        return 0.35
    return 0.95 if voted == 1.0 else 0.18


def draw_diffraction_spikes(ax, x: float, y: float, color: str,
                            intensity: float, base_size: float) -> None:
    """Render a 4-point cross spike for bright stars."""
    if intensity < 0.40:
        return
    L = (0.20 + 0.55 * intensity) * (base_size / 360) ** 0.4
    for w, a in [(2.6, 0.18), (1.5, 0.42), (0.7, 0.85)]:
        ax.plot([x - L, x + L], [y, y], color=color,
                lw=w * intensity, alpha=a * intensity,
                solid_capstyle="round", zorder=4)
        ax.plot([x, x], [y - L * 0.85, y + L * 0.85], color=color,
                lw=w * intensity, alpha=a * intensity,
                solid_capstyle="round", zorder=4)


def draw_star(ax, x, y, color, voted, year):
    """Draw a single star with halo, core, and (if voter) spikes + hot core
    + a faint warm-gold outer ring that visually marks voters."""
    base = YEAR_SIZE[year]
    b = brightness(voted)
    is_voter = (voted == 1.0)

    draw_diffraction_spikes(ax, x, y, color, b, base)

    # Voter-only outer ring: warm gold halo that clearly distinguishes voters
    if is_voter:
        ax.scatter(x, y, s=base * 9.0, color="#FFE8A8", alpha=0.07,
                   zorder=2, edgecolors="none")
        ax.scatter(x, y, s=base * 4.5, color="#FFE8A8", alpha=0.14,
                   zorder=3, edgecolors="none")

    # Standard color halo (major color)
    ax.scatter(x, y, s=base * 6.0, color=color, alpha=0.10 * b,
               zorder=3, edgecolors="none")
    ax.scatter(x, y, s=base * 3.0, color=color, alpha=0.20 * b,
               zorder=4, edgecolors="none")

    # Core disk
    ax.scatter(x, y, s=base, color=color,
               alpha=0.30 + 0.70 * b, zorder=5,
               edgecolors="white", linewidths=0.4 + 0.5 * b)

    # Hot white core for voters
    if b > 0.65:
        ax.scatter(x, y, s=base * 0.40, color="white",
                   alpha=(b - 0.65) * 2.8, zorder=6, edgecolors="none")

# ─── Figure ────────────────────────────────────────────────────────────
fig, ax = plt.subplots(figsize=(16, 9.5))
fig.patch.set_facecolor("#0B0E2A")
ax.set_facecolor("#0B0E2A")

# Decorative background stars — all small, uniformly dim.
# Density tapers toward the edges (galactic-plane feel) using a triangular dist.
bg_n = 380
bg_x = rng.uniform(0.4, SKY_X_MAX + 0.2, bg_n)
bg_y = rng.triangular(-Y_RANGE / 2 - 0.4, 0, Y_RANGE / 2 + 0.4, bg_n)
bg_s = rng.uniform(0.4, 1.8, bg_n)
ax.scatter(bg_x, bg_y, s=bg_s, color="white", alpha=0.12, zorder=1)

# Average-engagement reference line at y = 0 — clipped to the sky area so it
# doesn't bleed into the legend column. Soft white glow + a muted white dash.
HORIZON_X0, HORIZON_X1 = 0.18, SKY_X_MAX + 0.10
ax.plot([HORIZON_X0, HORIZON_X1], [0, 0], color="white",
        linewidth=4.0, alpha=0.08, zorder=0.55, solid_capstyle="round")
ax.plot([HORIZON_X0, HORIZON_X1], [0, 0], color="#D6DDEF",
        linewidth=1.2, alpha=0.70, zorder=0.6,
        linestyle=(0, (6, 4)))
ax.text(HORIZON_X1, 0.15, "average engagement",
        color="#D6DDEF", fontsize=9, va="bottom", ha="right",
        family="serif", fontstyle="italic", alpha=0.85)

# Y gridlines (engagement score)
for s in [-2, -1, 1, 2]:
    if SCORE_LO - 0.2 <= s <= SCORE_HI + 0.2:
        y = y_from_score(s)
        ax.axhline(y, color="#1A2148", linewidth=0.5, alpha=0.6, zorder=0.5)
        ax.text(0.20, y, f"{s:+d}", color="#566196", fontsize=8,
                ha="left", va="center", family="serif",
                fontstyle="italic", alpha=0.85)
ax.text(0.20, y_from_score(0), "0", color="#7B8AC9", fontsize=8.5,
        ha="left", va="center", family="serif", fontweight="bold", alpha=0.95)

# X gridlines (commute time)
for c, label in [(0, "0"), (30, "30 min"), (60, "1 hr"), (120, "2 hr"),
                 (180, "3 hr"), (240, "4 hr")]:
    x = x_from_commute(c)
    ax.plot([x, x], [-Y_RANGE / 2 - 0.2, Y_RANGE / 2 + 0.05],
            color="#1A2148", linewidth=0.45, alpha=0.55, zorder=0.5)
    ax.text(x, -Y_RANGE / 2 - 0.30, label, color="#566196",
            fontsize=8, ha="center", va="top",
            family="serif", fontstyle="italic", alpha=0.85)

# On-campus strip — a darker zone left of the main sky
ax.axvspan(ON_CAMPUS_X_CENTER - ON_CAMPUS_X_WIDTH / 2 - 0.15,
           ON_CAMPUS_X_CENTER + ON_CAMPUS_X_WIDTH / 2 + 0.15,
           ymin=0.05, ymax=0.95, color="#1A2148", alpha=0.4, zorder=0.4)
ax.text(ON_CAMPUS_X_CENTER, -Y_RANGE / 2 - 0.30, "on campus",
        color="#7B8AC9", fontsize=8.5, ha="center", va="top",
        family="serif", fontweight="bold", alpha=0.92)

# Divider between the on-campus strip and the open sky
ax.plot([2.15, 2.15], [-Y_RANGE / 2 - 0.05, Y_RANGE / 2 + 0.05],
        color="#3A4884", linewidth=0.5, alpha=0.45, linestyle="--", zorder=0.6)

# ─── Draw stars ────────────────────────────────────────────────────────
for _, r in pdf.sort_values("year").iterrows():
    color = PALETTE.get(r["major"], "#CCCCCC")
    draw_star(ax, r["x"], r["y"], color, r["voted"], r["year"])

# ─── Title block ───────────────────────────────────────────────────────
title_x = (0.6 + SKY_X_MAX) / 2
ax.text(title_x, Y_RANGE / 2 + 1.15, "CONSTELLATIONS OF THE CIVIC COMMUTE",
        ha="center", color="white", fontsize=24,
        family="serif", fontweight="bold")
ax.text(title_x, Y_RANGE / 2 + 0.65,
        "A portrait of 41 UH undergraduates — colored by major, sized by year,\n"
        "drifting outward with the commute, glowing with engagement",
        ha="center", color="#9FAACE", fontsize=10.5,
        family="serif", fontstyle="italic")

# ─── Axis labels ───────────────────────────────────────────────────────
ax.text(title_x, -Y_RANGE / 2 - 0.95,
        "COMMUTE TIME  (in minutes)",
        color="#9FAACE", fontsize=9.8, ha="center", va="top",
        family="serif", fontweight="bold")
ax.text(0.20, Y_RANGE / 2 + 0.45, "CIVIC ENGAGEMENT  (PCA index)",
        color="#9FAACE", fontsize=9.8, ha="left", va="bottom",
        family="serif", fontweight="bold")

# ─── Legend column ─────────────────────────────────────────────────────
LX = 12.65

ax.text(LX, Y_RANGE / 2 + 0.55, "MAJOR", color="#9FAACE", fontsize=10.5,
        family="serif", fontweight="bold", va="bottom")
for i, major in enumerate(MAJOR_ORDER):
    yy = Y_RANGE / 2 + 0.20 - i * 0.40
    color = PALETTE.get(major, "#CCCCCC")
    n = (pdf["major"] == major).sum()
    ax.scatter(LX + 0.10, yy, s=160, color=color, alpha=0.28, edgecolors="none")
    ax.scatter(LX + 0.10, yy, s=80, color=color, alpha=0.95,
               edgecolors="white", linewidths=0.4)
    pretty = display_major(major)
    label = (pretty if len(pretty) < 34 else pretty[:32] + "…") + f"  · n={n}"
    ax.text(LX + 0.40, yy, label, color="white", fontsize=8.6,
            va="center", family="serif")

# YEAR (size)
y_year_top = -1.05
ax.text(LX, y_year_top, "YEAR  (size)", color="#9FAACE", fontsize=10.5,
        family="serif", fontweight="bold", va="bottom")
for i, yr in enumerate([1, 2, 3, 4]):
    yy = y_year_top - 0.45 - i * 0.45
    ax.scatter(LX + 0.10, yy, s=YEAR_SIZE[yr], color="#C8D0F0",
               alpha=0.85, edgecolors="white", linewidths=0.4)
    suf = {1: "st", 2: "nd", 3: "rd", 4: "th"}[yr]
    ax.text(LX + 0.55, yy, f"{yr}{suf} year", color="white", fontsize=8.6,
            va="center", family="serif")

# VOTING (glow + spikes)
y_vote_top = -3.30
ax.text(LX, y_vote_top, "VOTED IN MARCH PRIMARY",
        color="#9FAACE", fontsize=10.5,
        family="serif", fontweight="bold", va="bottom")
for i, (b_val, lbl) in enumerate([(0.95, "voted"), (0.18, "did not vote")]):
    xx = LX + 0.45 + i * 1.10
    yy = y_vote_top - 0.55
    if b_val > 0.40:
        L = (0.20 + 0.55 * b_val) * 0.35
        for w, a in [(1.5, 0.42), (0.7, 0.85)]:
            ax.plot([xx - L, xx + L], [yy, yy], color="#E8D88B",
                    lw=w * b_val, alpha=a * b_val,
                    solid_capstyle="round", zorder=4)
            ax.plot([xx, xx], [yy - L * 0.85, yy + L * 0.85], color="#E8D88B",
                    lw=w * b_val, alpha=a * b_val,
                    solid_capstyle="round", zorder=4)
    ax.scatter(xx, yy, s=240, color="#E8D88B",
               alpha=0.10 * b_val, edgecolors="none")
    ax.scatter(xx, yy, s=110, color="#E8D88B",
               alpha=0.30 + 0.70 * b_val,
               edgecolors="white", linewidths=0.4 + 0.5 * b_val)
    if b_val > 0.65:
        ax.scatter(xx, yy, s=33, color="white",
                   alpha=(b_val - 0.65) * 2.5, edgecolors="none")
    ax.text(xx, yy - 0.45, lbl, color="#9FAACE", fontsize=8.4,
            ha="center", family="serif", fontstyle="italic")

# ─── Source line ───────────────────────────────────────────────────────
ax.text(title_x, -Y_RANGE / 2 - 1.55,
        f"n = {len(pdf)}  |  Source: UH Undergraduate Student Life Survey, Spring 2026",
        ha="center", color="#7B8AC9", fontsize=8.8,
        family="serif", fontstyle="italic", alpha=0.85)

# ─── Frame ─────────────────────────────────────────────────────────────
ax.set_xlim(-0.05, 15.5)
ax.set_ylim(-Y_RANGE / 2 - 1.95, Y_RANGE / 2 + 1.55)
ax.set_xticks([])
ax.set_yticks([])
for spine in ax.spines.values():
    spine.set_visible(False)

plt.tight_layout()
OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
plt.savefig(OUT_PATH, dpi=160, bbox_inches="tight", facecolor="#0B0E2A")
print(f"Saved: {OUT_PATH}")
