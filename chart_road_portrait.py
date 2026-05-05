import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
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

# ── Canvas ────────────────────────────────────────────────────────────────────

BG         = "#0E0E0E"
ROAD       = "#2C2C2C"
ROAD_EDGE  = "#3A3A3A"
DASH       = "#C8A800"
WHITE      = "#EEEEEE"
SCARLET    = "#CC0000"
SOFT       = "#888888"
GOLD       = "#F6BE00"

fig, ax = plt.subplots(figsize=(22, 11))
fig.patch.set_facecolor(BG)
ax.set_facecolor(BG)
ax.set_xlim(0, 22)
ax.set_ylim(0, 11)
ax.axis("off")

# ── Layout constants ──────────────────────────────────────────────────────────

X0       = 0.8    # trunk start
X_SPLIT1 = 4.0    # on/off campus fork
X_SPLIT2 = 7.5    # transport mode fork
X_ROAD   = 9.2    # where horizontal mode roads begin
X_MAX    = 21.5   # end of longest road (METRO Bus)

Y_TRUNK  = 5.5    # main trunk y
Y_ON     = 9.2    # on-campus branch y

# Mode y positions — evenly spaced, METRO Bus highest (longest road)
MODE_YS = {
    "METRO Bus":    8.0,
    "Personal Car": 7.0,
    "Walking":      6.0,
    "Bicycle":      4.8,
    "Carpool":      3.8,
    "METRO Rail":   2.8,
}

max_commute  = commute["avg"].max()
commute_scale = (X_MAX - X_ROAD) / max_commute   # x-units per minute

# ── Drawing helpers ───────────────────────────────────────────────────────────

def s_curve(x0, y0, x1, y1, n_pts=300):
    """Smooth S-curve bezier: leaves x0,y0 horizontally, arrives x1,y1 horizontally."""
    cx = (x0 + x1) / 2
    t  = np.linspace(0, 1, n_pts)
    bx = (1-t)**3*x0 + 3*(1-t)**2*t*cx + 3*(1-t)*t**2*cx + t**3*x1
    by = (1-t)**3*y0 + 3*(1-t)**2*t*y0 + 3*(1-t)*t**2*y1 + t**3*y1
    return bx, by

def road(ax, xs, ys, lw, color=ROAD, zorder=2):
    """Draw a road: edge shadow, road surface, center dashes."""
    ax.plot(xs, ys, color=ROAD_EDGE, linewidth=lw + 3,
            solid_capstyle="butt", zorder=zorder - 1)
    ax.plot(xs, ys, color=color, linewidth=lw,
            solid_capstyle="butt", zorder=zorder)
    ax.plot(xs, ys, color=DASH, linewidth=max(0.5, lw * 0.07),
            linestyle=(0, (7, 7)), solid_capstyle="butt", zorder=zorder + 1)

def trunk_lw(n):
    return max(6, (n / n_all) * 42)

def mode_lw(n):
    return max(5, (n / commute["n"].max()) * 22)

# ── 1. Main trunk ─────────────────────────────────────────────────────────────
road(ax, [X0, X_SPLIT1], [Y_TRUNK, Y_TRUNK], trunk_lw(n_all))

# ── 2. On-campus fork ─────────────────────────────────────────────────────────
bx, by = s_curve(X_SPLIT1, Y_TRUNK, X_SPLIT1 + 2.2, Y_ON)
road(ax, bx, by, trunk_lw(n_on))
road(ax, [X_SPLIT1 + 2.2, X_SPLIT1 + 4.0], [Y_ON, Y_ON], trunk_lw(n_on))

# Dead-end / campus marker
ax.plot(X_SPLIT1 + 4.0, Y_ON, "o", color=SCARLET, markersize=14, zorder=6)
ax.text(X_SPLIT1 + 4.4, Y_ON + 0.05, "🏫", fontsize=20,
        fontfamily=EMOJI_FONT, va="center", ha="left", zorder=6)
ax.text(X_SPLIT1 + 5.6, Y_ON + 0.25, "ON-CAMPUS", fontsize=10,
        color=SCARLET, fontweight="bold", va="center", zorder=6)
ax.text(X_SPLIT1 + 5.6, Y_ON - 0.3, f"n = {n_on}  ·  no commute",
        fontsize=9, color=SOFT, va="center", zorder=6)

# ── 3. Off-campus trunk to mode fork ─────────────────────────────────────────
bx, by = s_curve(X_SPLIT1, Y_TRUNK, X_SPLIT2, Y_TRUNK)
road(ax, bx, by, trunk_lw(n_off))

# ── 4. Transport mode roads ───────────────────────────────────────────────────
for mode, row in commute.iterrows():
    y    = MODE_YS.get(mode, Y_TRUNK)
    n    = int(row["n"])
    avg  = row["avg"]
    lw   = mode_lw(n)
    x_end = X_ROAD + avg * commute_scale
    emoji = EMOJI_MAP.get(mode, "")
    is_longest = (avg == commute["avg"].max())

    # S-curve connector from mode fork to mode y
    bx, by = s_curve(X_SPLIT2, Y_TRUNK, X_ROAD, y)
    road(ax, bx, by, lw)

    # Horizontal road — length = commute time
    road(ax, [X_ROAD, x_end], [y, y], lw,
         color="#3D1010" if is_longest else ROAD)

    # End marker: emoji
    ax.text(x_end + 0.15, y, emoji, fontsize=20,
            fontfamily=EMOJI_FONT, va="center", ha="left", zorder=6)

    # Mode name (left of s-curve)
    ax.text(X_SPLIT2 - 0.15, y, mode, fontsize=9.5, color=WHITE,
            va="center", ha="right", fontweight="bold", zorder=6)

    # Commute time label at road end
    label_color = GOLD if is_longest else SOFT
    ax.text(x_end + 1.3, y + 0.28,
            f"{avg:.0f} min", fontsize=10, color=label_color,
            fontweight="bold", va="center", zorder=6)
    ax.text(x_end + 1.3, y - 0.28,
            f"n = {n}", fontsize=8.5, color=SOFT, va="center", zorder=6)

# ── 5. Fork labels ────────────────────────────────────────────────────────────
for x, label in [(X_SPLIT1, "Where do\nyou live?"),
                 (X_SPLIT2, "How do you\nget there?")]:
    ax.text(x, 1.9, label, fontsize=8.5, color=SOFT,
            ha="center", va="top", style="italic", linespacing=1.5)
    ax.plot([x, x], [2.35, Y_TRUNK - trunk_lw(n_all)*0.018],
            color="#444444", linewidth=0.8, linestyle="--", zorder=1)

# ── 6. Time scale bar ─────────────────────────────────────────────────────────
scale_y = 1.3
ax.text(X_ROAD, scale_y + 0.35, "← road length = average commute time →",
        fontsize=8, color=SOFT, ha="left", style="italic")
for mins in [0, 30, 60, 90, 120]:
    sx = X_ROAD + mins * commute_scale
    ax.plot([sx, sx], [scale_y - 0.12, scale_y + 0.12],
            color=SOFT, linewidth=1, zorder=5)
    ax.text(sx, scale_y - 0.22, f"{mins}m", fontsize=8,
            color=SOFT, ha="center", va="top")
ax.plot([X_ROAD, X_MAX], [scale_y, scale_y], color="#333333", linewidth=1)

# ── 7. Title & source ─────────────────────────────────────────────────────────
ax.text(11, 10.6, "The Road to Campus",
        fontsize=26, fontweight="bold", color=WHITE, ha="center", va="center")
ax.text(11, 10.1,
        "UH students' commute by residence and mode of transportation",
        fontsize=12, color=SOFT, ha="center", va="center", style="italic")
ax.text(11, 0.25,
        f"n = {n_all}  ·  Source: UH Undergraduate Student Life Survey, Spring 2026",
        fontsize=8.5, color=SOFT, ha="center", va="bottom", style="italic")

plt.savefig("chart_road_portrait.png", bbox_inches="tight",
            facecolor=BG, dpi=150)
plt.close()
print("Saved chart_road_portrait.png")
