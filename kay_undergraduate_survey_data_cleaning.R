########### LOADING DATASET ######################

library(readr)

Undergraduate_Student_Life_Survey_April_23_2026 <- read_csv("Undergraduate_Student_Life_Survey_April 23-2026.csv")
View(Undergraduate_Student_Life_Survey_April_23_2026)
undergrad_survey <- Undergraduate_Student_Life_Survey_April_23_2026

############ SUBSUBSETTING #########################


# Selecting needed variables "Finished" and responses for Q1 - Q16
undergrad_survey <- undergrad_survey[c(6,8,11:30)]

# Deleting rows from people whose "Finished" = False AND where Q1 (undergraduate 
# student status) = No or NA

library(dplyr)

undergrad_survey <- filter(undergrad_survey, Finished!="False" & Q1!= "No")

# deleting rows 1 and 2 (these rows are the question and importing information) both are unneeded

undergrad_survey <- undergrad_survey[-c(1,2), ] 


