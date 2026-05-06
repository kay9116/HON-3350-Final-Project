# Author: Vincent Wren
# All analytical decisions, variable selections, and coding logic were conceived by myself.
# Claude AI (Anthropic) was used as a technical assistant to help implement and debug the code.
# No ideation, interpretation, or analytical judgment was performed by AI.

import pandas as pd

df = pd.read_csv("../data/raw/Undergraduate_Student_Life_Survey_April 23-2026.csv")

df = df[["Finished", "ResponseId", "Q1", "Q2", "Q3", "Q4", "Q5",
         "Q6", "Q7", "Q15", "Q8_1", "Q8_2", "Q8_3", "Q8_4", "Q8_5",
         "Q9", "Q10", "Q10_7_TEXT", "Q11", "Q12", "Q13", "Q16"]]

# Deleting rows 0 and 1 

df = df.drop(index=[0, 1]).reset_index(drop=True)

# Filtering out incomplete responses and non-undergraduates

df = df[(df["Finished"] != "False") & (df["Q1"] == "Yes")].reset_index(drop=True)


# transportation columns only

## On-campus = 1, Off-Campus = 0

df["Q9"] = df["Q9"].map({"On-campus": 1, "Off-Campus": 0})


## Q10 (primary mode of transportation to campus)
transport_modes = {
    "Personal Car": 1,
    "Carpool":      2,
    "METRO Bus":    3,
    "METRO Rail":   4,
    "Bicycle":      5,
    "Walking":      6,
    "Other":        7,
}

q10_index = df.columns.get_loc("Q10")
df.insert(q10_index + 1, "Q10 ID", df["Q10"].map(transport_modes))


## Q11 (estimated total commute time in minutes): convert to numeric

df["Q11"] = pd.to_numeric(df["Q11"], errors="coerce")


## Q12 (number of METRO transfers): convert to numeric

df["Q12"] = pd.to_numeric(df["Q12"], errors="coerce")


## Q13 (owns a private vehicle): Yes = 1, No = 0

df["Q13"] = df["Q13"].map({"Yes": 1, "No": 0})


## Q16 (how often providing carpooling services): recode using same frequency scale as Q8
# 0 = "Not at all", 6 = "Basically every day"

frequency_lookup = {
    "Basically every day":    6,
    "A few times a week":     5,
    "A few times a month":    4,
    "Once a month":           3,
    "Less than once a month": 2,
    "Once":                   1,
    "Not at all":             0,
}

df["Q16"] = df["Q16"].str.strip().map(frequency_lookup)



df.to_csv("../data/cleaned/cleaned_transportation_survey.csv", index=False)

print("Done. cleaned_transportation_survey.csv has been saved.")
