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

---

## Civic Engagement (Kaylee Gallardo)

### Data Cleaning
- Filtered to confirmed undergraduates (Q1 = "Yes") with complete responses, and removed respondents whose Finished = "False"
- Re-coded Q4 (voted), Q6 (attended a university town hall), Q7 (protested on campus), Q15 (left campus for a protest) to be binary: Yes = 1, No = 0
- Re-coded Q3 (majors) into numeric values based on alphabetical order: Architecture = 1, Art, Music, Dance, Theater = 2, ... and so forth
- Re-coded Q8 matrix (Q8.1 - Q8.5) into ordinal, numeric values: Not at all = 1, Basically Everyday = 7 ...etc
- Created a separate data frame for Q5 that separates cells with multiple values for accurate counts
- Re-coded Q5 answers into numeric values (1-8) to denote reason why the respondent didn't vote on campus
- Final cleaned dataset: **41 respondents**


### Visualizations

**`charts/demographics_by_major.png`** - Displays the distribution of respondents by major and color codes each to denote their years in college

**`charts/demographics_by_class.png`** - Displays the distribution of respondents by years in college and color codes each to denote their major

**`charts/Q4_plot.png`** - Split bar graph that depicts the proportion of respondents that did and did not vote on campus for the March Primaries (including 1 who did not respond)

**`charts/Q5_bar_graph.png`** - Bar graph that shows the proportion of people that chose each reason as why they did not vote on campus

**`charts/Q5&4_barplot.png`** Split bar graph that adds individuals who chose either reason 2 or 7 to see the proportion of respondents that voted whether on campus or not

---
