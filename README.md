# HON-3350-Final-Project

**The Road to Campus: Commute Length and Civic Participation Among UH Undergraduates**

A data portrait project examining whether commute burden affects civic engagement among University of Houston undergraduate students. Data collected via original survey, Spring 2026.

---

## Team

- **Kaylee Gallardo** — Civic Engagement (Q1–Q8)
- **Vincent Wren** — Transportation (Q9–Q16)

---

## Repository Structure

| Path | Description |
|------|-------------|
| `data/raw/Undergraduate_Student_Life_Survey_April 23-2026.csv` | Raw survey data |
| `data/cleaned/cleaned_transportation_survey.csv` | Cleaned transportation data (Vincent) |
| `data/cleaned/civic_engagement_cleaned_undergraduate_survey.csv` | Cleaned civic engagement data (Kaylee) |
| `scripts/vincent_transportation_survey_data_cleaning.py` | Cleans and recodes transportation variables (Q9–Q16) |
| `scripts/vincent_transportation_visualizations.py` | Generates 4 individual transportation charts |
| `scripts/chart_road_portrait.py` | Generates the road map portrait visualization |
| `scripts/kay_undergraduate_survey_data_cleaning.R` | Cleans and recodes civic engagement variables (Q1–Q8) |
| `charts/` | All output visualizations (PNG) |

---

## Transportation Side (Vincent Wren)

### Data Cleaning
- Filtered to confirmed undergraduates (Q1 = "Yes") with complete responses
- Recoded Q9 (residence) to binary: on-campus = 1, off-campus = 0
- Recoded Q10 (transport mode) with numeric IDs; preserved original text column
- Converted Q11 (commute time) and Q12 (METRO transfers) to numeric
- Recoded Q13 (vehicle ownership) to binary: Yes = 1, No = 0
- Recoded Q16 (carpooling frequency) to 0–6 scale matching Kaylee's civic engagement frequency coding
- Final cleaned dataset: **41 respondents**

### Visualizations

**`charts/chart1_residence.png`** — Proportional bar showing on-campus (n=20) vs. off-campus (n=21) split with large bold counts and percentages.

**`charts/chart2_transport_mode.png`** — Isotype chart where each transportation emoji represents one student, arranged by mode. Visually conveys both count and distribution at a glance.

**`charts/chart3_commute_time.png`** — Dot histogram where each 🧍 represents one off-campus student, stacked in 20-minute bins. Includes gold median line and gray mean line to show outlier effect.

**`charts/chart4_commute_by_mode.png`** — Lollipop chart of average commute time by transport mode, with the mode's emoji at the tip of each line. Highlights that METRO Bus riders face the longest average commute (142 min).

**`charts/chart_road_portrait.png`** — Final visualization. A road map where all 41 students enter as one trunk road, fork by residence (on/off-campus), then branch again via transport mode. Road width is proportional to number of respondents; road length is proportional to average commute time. Each mode has a distinct road style: train tracks for METRO Rail, green bike lane for Bicycle, red bus lane for METRO Bus, and standard asphalt roads with yellow dashes for Car and Carpool.
