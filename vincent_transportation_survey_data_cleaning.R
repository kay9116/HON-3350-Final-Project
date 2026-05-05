# LOADING DATASET

library(readr)

Undergraduate_Student_Life_Survey_April_23_2026 <- read_csv("Undergraduate_Student_Life_Survey_April 23-2026.csv")

undergrad_survey <- Undergraduate_Student_Life_Survey_April_23_2026

#SUBSETTING 

# Selecting Finished, ResponseId, Q1 (for filtering), and transportation questions Q9-Q16
# Column indices: Finished(6), ResponseId(8), Q1(11), Q9(24), Q10(25), Q10_7_TEXT(26),
#                 Q11(27), Q12(28), Q13(29), Q16(30)

undergrad_survey <- undergrad_survey[c(6, 8, 11, 24:30)]

# Deleting rows where "Finished" = False AND where Q1 (undergraduate student status) = No or NA

library(dplyr)

undergrad_survey <- filter(undergrad_survey, Finished != "False" & Q1 != "No")

# Deleting rows 1 and 2 (question text and import metadata rows — not actual responses)

undergrad_survey <- undergrad_survey[-c(1, 2), ]


################ VARIABLE RECODING ###########################

library(tidyverse)

## Q9 (on-campus vs off-campus residence): On-campus = 1, Off-Campus = 0

undergrad_survey <- undergrad_survey %>%
  mutate(Q9 = case_when(
    Q9 == "On-campus"  ~ 1,
    Q9 == "Off-Campus" ~ 0
  ))


## Q10 (primary mode of transportation to campus)

# Observations assigned a numeric code based on the order they appeared in the survey
# (Personal Car, Carpool, METRO Bus, METRO Rail, Bicycle, Walking, Other)
# verify this order matches the actual survey if re-running

transport_modes <- data.frame(mode = c("Personal Car",
                                       "Carpool",
                                       "METRO Bus",
                                       "METRO Rail",
                                       "Bicycle",
                                       "Walking",
                                       "Other"))

transport_modes$order_id <- seq_len(nrow(transport_modes))

undergrad_survey <- undergrad_survey %>%
  left_join(transport_modes, by = c("Q10" = "mode")) %>%
  mutate(`Q10 ID` = order_id, .after = Q10) %>%
  select(-order_id)


## Q11 (estimated total commute time in minutes): convert from character to numeric

undergrad_survey <- undergrad_survey %>%
  mutate(Q11 = as.numeric(Q11))


## Q12 (number of METRO transfers): convert from character to numeric

undergrad_survey <- undergrad_survey %>%
  mutate(Q12 = as.numeric(Q12))


## Q13 (owns a private vehicle): Yes = 1, No = 0
# Applies only to columns where every observation is "Yes", "No", or NA
# (catches Q13; leaves other columns untouched)

undergrad_survey <- undergrad_survey %>%
  mutate(across(where(~ all(. %in% c("Yes", "No", NA))),
                ~ if_else(. == "Yes", 1, 0)))

## Q16 (how often providing carpooling services): recode using same frequency scale as Q8

frequency_codes <- data.frame(frequency = c("Basically every day",
                                             "A few times a week",
                                             "A few times a month",
                                             "Once a month",
                                             "Less than once a month",
                                             "Once",
                                             "Not at all"))

# assigns numbers 0-6 (0 = "Not at all", 6 = "Basically every day")

frequency_codes$order_id <- rev(seq_len(nrow(frequency_codes)))

library(stringr)

undergrad_survey <- undergrad_survey %>%
  mutate(Q16 = str_squish(as.character(Q16)))

frequency_codes <- frequency_codes %>%
  mutate(frequency = str_squish(as.character(frequency)))

lookup <- setNames(frequency_codes$order_id, frequency_codes$frequency)

undergrad_survey <- undergrad_survey %>%
  mutate(Q16 = lookup[Q16])

# writing the cleaned dataset to a CSV file
# visualizations and analysis will be done using this file

write_csv(undergrad_survey, "cleaned_transportation_survey.csv")
