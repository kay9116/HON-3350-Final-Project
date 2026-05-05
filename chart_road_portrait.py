# Author: Vincent Wren
# All chart concepts, design choices, and analytical decisions were conceived by myself.
# Claude AI (Anthropic) was used as a technical assistant to help implement and debug the code.
# No ideation, interpretation, or analytical judgment was performed by AI.

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

df = pd.read_csv("cleaned_transportation_survey.csv")

# ── Data ──────────────────────────────────────────────────────────────────────

q9    = df["Q9"].dropna()
n_on  = int((q9 == 1).sum())
n_off = int((q9 == 0).sum())
n_all = n_on + n_off

commute = (
    df.dropna(subset=["Q10", "Q11"])
    .groupby("Q10")["Q11"]
    .agg(["mean", "count"])
    .rename(columns={"mean": "avg", "count": "n"})
    .sort_values("avg", ascending=False)
)

EMOJI_MAP  = {
    "Personal Car": "🚗", "METRO Bus": "🚌", "Carpool": "🚐",
    "METRO Rail": "🚇", "Bicycle": "🚲", "Walking": "🚶",
}
EMOJI_FONT = "Segoe UI Emoji"

# ── Colors ────────────────────────────────────────────────────────────────────

BG         = "#F5F3EF"       # warm off-white
ROAD       = "#383838"
ROAD_EDGE  = "#1C1C1C"
DASH_YELL  = "#C8A800"
WHITE      = "#1A1A1A"       # near-black for text on light bg
SCARLET    = "#CC0000"
SOFT       = "#555555"
GOLD       = "#B08800"       # darker gold readable on light bg

# Per-mode road palettes  (surface, edge, mark_color, mark_style)
MODE_STYLE = {
    "Personal Car": ("#383838", "#1C1C1C", DASH_YELL,  (0, (7, 7))),
    "Carpool":      ("#383838", "#1C1C1C", DASH_YELL,  (0, (7, 7))),
    "METRO Bus":    ("#6B0000", "#3A0000", "#FFFFFF",   (0, (7, 7))),
    "Bicycle":      ("#1A5C1A", "#0D3A0D", "#FFFFFF",   (0, (3, 9))),
    "Walking":      ("#4A4540", "#2A2520", "#AAAAAA",   (0, (2, 14))),
    "METRO Rail":   ("#5C4033", "#3D2B1F", None,        None),
}

# ── Canvas ────────────────────────────────────────────────────────────────────

fig, ax = plt.subplots(figsize=(22, 11))
fig.patch.set_facecolor(BG)
ax.set_facecolor(BG)
ax.set_xlim(0, 22)
ax.set_ylim(0, 11)
ax.axis("off")

# ── Layout ────────────────────────────────────────────────────────────────────

X0       = 0.8
X_SPLIT1 = 4.0
X_SPLIT2 = 7.5
X_ROAD   = 9.2
X_MAX    = 21.5
Y_TRUNK  = 5.5
Y_ON     = 9.2

MODE_YS = {
    "METRO Bus":    8.0,
    "Personal Car": 7.0,
    "Walking":      6.0,
    "Bicycle":      4.8,
    "Carpool":      3.8,
    "METRO Rail":   2.8,
}

max_commute   = commute["avg"].max()
commute_scale = (X_MAX - X_ROAD) / max_commute

def trunk_lw(n): return max(6, (n / n_all) * 42)
def mode_lw(n):  return max(7, (n / commute["n"].max()) * 22)

# ── Bezier helper ─────────────────────────────────────────────────────────────

def s_curve(x0, y0, x1, y1, n_pts=300):
    cx = (x0 + x1) / 2
    t  = np.linspace(0, 1, n_pts)
    bx = (1-t)**3*x0 + 3*(1-t)**2*t*cx + 3*(1-t)*t**2*cx + t**3*x1
    by = (1-t)**3*y0 + 3*(1-t)**2*t*y0 + 3*(1-t)*t**2*y1 + t**3*y1
    return bx, by

# ── Road drawing functions ────────────────────────────────────────────────────

def road_standard(ax, xs, ys, lw, surf, edge, mark_col, mark_style, zorder=2):
    ax.plot(xs, ys, color=edge, linewidth=lw + 4,
            solid_capstyle="butt", zorder=zorder - 1)
    ax.plot(xs, ys, color=surf, linewidth=lw,
            solid_capstyle="butt", zorder=zorder)
    if mark_col and mark_style:
        ax.plot(xs, ys, color=mark_col, linewidth=max(0.6, lw * 0.07),
                linestyle=mark_style, solid_capstyle="butt", zorder=zorder + 1)

def road_train_track(ax, x0, x1, y, lw, zorder=2):
    """Ballast bed + wooden ties + steel rails."""
    # Ballast
    ax.plot([x0, x1], [y, y], color="#3D2B1F", linewidth=lw + 4,
            solid_capstyle="butt", zorder=zorder - 1)
    ax.plot([x0, x1], [y, y], color="#5C4033", linewidth=lw,
            solid_capstyle="butt", zorder=zorder)
    # Wooden cross ties
    dy_tie = 0.11
    for xt in np.arange(x0 + 0.15, x1, 0.28):
        ax.plot([xt, xt], [y - dy_tie, y + dy_tie],
                color="#6B4C34", linewidth=4.5,
                solid_capstyle="butt", zorder=zorder + 1)
    # Steel rails
    dy_rail = 0.055
    for dy in [+dy_rail, -dy_rail]:
        ax.plot([x0, x1], [y + dy, y + dy],
                color="#BBBBBB", linewidth=2.2,
                solid_capstyle="butt", zorder=zorder + 2)

def draw_road(ax, xs, ys, mode, lw, zorder=2):
    """Route to the right road-drawing function based on mode."""
    if mode == "METRO Rail":
        # Train track only works cleanly on straight segments
        road_train_track(ax, xs[0], xs[-1], ys[0], lw, zorder)
    else:
        surf, edge, mc, ms = MODE_STYLE[mode]
        road_standard(ax, xs, ys, lw, surf, edge, mc, ms, zorder)

def draw_connector(ax, x0, y0, x1, y1, mode, lw, zorder=2):
    """S-curve connector — uses standard style for all modes."""
    bx, by = s_curve(x0, y0, x1, y1)
    surf, edge = MODE_STYLE[mode][:2]
    ax.plot(bx, by, color=edge, linewidth=lw + 4,
            solid_capstyle="butt", zorder=zorder - 1)
    ax.plot(bx, by, color=surf, linewidth=lw,
            solid_capstyle="butt", zorder=zorder)

# ── 1. Main trunk ─────────────────────────────────────────────────────────────
lw_all = trunk_lw(n_all)
ax.plot([X0, X_SPLIT1], [Y_TRUNK, Y_TRUNK],
        color=ROAD_EDGE, linewidth=lw_all + 4, solid_capstyle="butt", zorder=1)
ax.plot([X0, X_SPLIT1], [Y_TRUNK, Y_TRUNK],
        color=ROAD, linewidth=lw_all, solid_capstyle="butt", zorder=2)
ax.plot([X0, X_SPLIT1], [Y_TRUNK, Y_TRUNK],
        color=DASH_YELL, linewidth=max(0.8, lw_all * 0.07),
        linestyle=(0, (7, 7)), solid_capstyle="butt", zorder=3)

# ── 2. On-campus fork ─────────────────────────────────────────────────────────
lw_on = trunk_lw(n_on)
bx, by = s_curve(X_SPLIT1, Y_TRUNK, X_SPLIT1 + 2.2, Y_ON)
ax.plot(bx, by, color=ROAD_EDGE, linewidth=lw_on + 4, solid_capstyle="butt", zorder=1)
ax.plot(bx, by, color=ROAD,      linewidth=lw_on,     solid_capstyle="butt", zorder=2)
ax.plot(bx, by, color=DASH_YELL, linewidth=max(0.6, lw_on * 0.07),
        linestyle=(0, (7, 7)), solid_capstyle="butt", zorder=3)

# Short dead-end road
ax.plot([X_SPLIT1 + 2.2, X_SPLIT1 + 3.8], [Y_ON, Y_ON],
        color=ROAD_EDGE, linewidth=lw_on + 4, solid_capstyle="butt", zorder=1)
ax.plot([X_SPLIT1 + 2.2, X_SPLIT1 + 3.8], [Y_ON, Y_ON],
        color=ROAD,      linewidth=lw_on,     solid_capstyle="butt", zorder=2)
ax.plot([X_SPLIT1 + 3.8], [Y_ON], "o", color=SCARLET, markersize=13, zorder=6)

ax.text(X_SPLIT1 + 4.15, Y_ON + 0.05, "🏫",
        fontsize=20, fontfamily=EMOJI_FONT, va="center", ha="left", zorder=6)
ax.text(X_SPLIT1 + 5.4, Y_ON + 0.28, "ON-CAMPUS",
        fontsize=10, color=SCARLET, fontweight="bold", va="center", zorder=6)
ax.text(X_SPLIT1 + 5.4, Y_ON - 0.25, f"n = {n_on}  ·  no commute",
        fontsize=9,  color=SOFT,    va="center", zorder=6)

# ── 3. Off-campus trunk ───────────────────────────────────────────────────────
lw_off = trunk_lw(n_off)
bx, by = s_curve(X_SPLIT1, Y_TRUNK, X_SPLIT2, Y_TRUNK)
ax.plot(bx, by, color=ROAD_EDGE, linewidth=lw_off + 4, solid_capstyle="butt", zorder=1)
ax.plot(bx, by, color=ROAD,      linewidth=lw_off,     solid_capstyle="butt", zorder=2)
ax.plot(bx, by, color=DASH_YELL, linewidth=max(0.6, lw_off * 0.07),
        linestyle=(0, (7, 7)), solid_capstyle="butt", zorder=3)

# ── 4. Transport mode roads ───────────────────────────────────────────────────
for mode, row in commute.iterrows():
    y     = MODE_YS.get(mode, Y_TRUNK)
    n     = int(row["n"])
    avg   = row["avg"]
    lw    = mode_lw(n)
    x_end = X_ROAD + avg * commute_scale
    emoji = EMOJI_MAP.get(mode, "")
    is_max = avg == commute["avg"].max()

    # S-curve connector
    draw_connector(ax, X_SPLIT2, Y_TRUNK, X_ROAD, y, mode, lw, zorder=2)

    # Horizontal road — length ∝ commute time
    draw_road(ax, [X_ROAD, x_end], [y, y], mode, lw, zorder=2)

    # Emoji at road tip
    ax.text(x_end + 0.15, y, emoji, fontsize=20,
            fontfamily=EMOJI_FONT, va="center", ha="left", zorder=6)

    # Mode name label (left of fork)
    ax.text(X_SPLIT2 - 0.2, y, mode, fontsize=9.5, color=WHITE,
            va="center", ha="right", fontweight="bold", zorder=6)

    # Commute stats — single tight line right after emoji
    label_col = GOLD if is_max else SOFT
    ax.text(x_end + 1.05, y + 0.17, f"{avg:.0f} min",
            fontsize=10, color=label_col, fontweight="bold", va="center", zorder=6)
    ax.text(x_end + 1.05, y - 0.17, f"n = {n}",
            fontsize=8.5, color=SOFT, va="center", zorder=6)

# ── 5. Fork annotations ───────────────────────────────────────────────────────
for x, label in [(X_SPLIT1, "Where do\nyou live?"),
                 (X_SPLIT2, "How do you\nget there?")]:
    ax.text(x, 1.85, label, fontsize=8.5, color=SOFT,
            ha="center", va="top", style="italic", linespacing=1.5)
    ax.plot([x, x], [2.3, Y_TRUNK - 0.35],
            color="#888888", linewidth=0.8, linestyle="--", zorder=1)

# ── 6. Time scale bar ─────────────────────────────────────────────────────────
sy = 1.2
ax.text(X_ROAD, sy + 0.55,
        "← road length = average commute time →",
        fontsize=8, color=SOFT, ha="left", style="italic")
ax.text(X_ROAD, sy + 0.35,
        "road width = number of respondents",
        fontsize=8, color=SOFT, ha="left", style="italic")
ax.plot([X_ROAD, X_MAX], [sy, sy], color="#888888", linewidth=1)
for mins in [0, 30, 60, 90, 120]:
    sx = X_ROAD + mins * commute_scale
    ax.plot([sx, sx], [sy - 0.12, sy + 0.12], color=SOFT, linewidth=1)
    ax.text(sx, sy - 0.22, f"{mins}m", fontsize=8,
            color=SOFT, ha="center", va="top")

# ── 7. Title & source ─────────────────────────────────────────────────────────
ax.text(11, 10.6, "The Road to Campus",
        fontsize=26, fontweight="bold", color=WHITE, ha="center")
ax.text(11, 10.1,
        "UH students' commute by residence and mode of transportation",
        fontsize=12, color=SOFT, ha="center", style="italic")
ax.text(11, 0.22,
        f"n = {n_all}  ·  Source: UH Undergraduate Student Life Survey, Spring 2026",
        fontsize=8.5, color=SOFT, ha="center", style="italic")

plt.savefig("chart_road_portrait.png", bbox_inches="tight",
            facecolor=BG, dpi=150)
plt.close()
print("Saved chart_road_portrait.png")
