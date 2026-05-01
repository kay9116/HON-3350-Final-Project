########### LOADING DATASET ######################

library(readr)

Undergraduate_Student_Life_Survey_April_23_2026 <- read_csv("Undergraduate_Student_Life_Survey_April 23-2026.csv")
View(Undergraduate_Student_Life_Survey_April_23_2026)
undergrad_survey <- Undergraduate_Student_Life_Survey_April_23_2026

############ SUBSUBSETTING #########################


# Selecting needed variables "Finished", Response ID, and responses for Q1 - Q16
undergrad_survey <- undergrad_survey[c(6,8,11:30)]

# Deleting rows from people whose "Finished" = False AND where Q1 (undergraduate 
# student status) = No or NA

library(dplyr)

undergrad_survey <- filter(undergrad_survey, Finished!="False" & Q1!= "No")

# deleting rows 1 and 2 (these rows are the question and importing information) both are unneeded

undergrad_survey <- undergrad_survey[-c(1,2), ] 


################ VARIABLE RECODING ###########################

library(tidyverse)

## Mutating observations "YES" and "NO" as 1 and 0
## Yes will now = 1 and No will = 0

# ***********************************************************************************
# the code below was written with the help of AI (ChatGPT)

# It runs the condition of if a cell contains "Yes" then it will be 
# changed to 1, if it is not "Yes" then the cell will be changed to 0 (NA is left unchanged)
# this condition is ONLY applied on columns where the WHOLE column contains ONLY observations "Yes", "No" or NA


undergrad_survey <- undergrad_survey %>%
  mutate(across(where(~ all(. %in% c("Yes", "No", NA))),
                ~ if_else(. == "Yes", 1, 0)))
# ***********************************************************************************


## Mutating Q2 (what year are you?)


# mutating "1 / 1st year" and so forth into simply 1,2,3,....

(undergrad_survey <- undergrad_survey %>%
    mutate(Q2 = case_when(
      
      Q2==  "1 / 1st year" ~ 1,
      
      Q2==   "2 / 2nd year" ~ 2,
      
      Q2==  "3 / 3rd year" ~ 3,
      
      Q2==   "4+ / 4th year and beyond" ~ 4
    )))


