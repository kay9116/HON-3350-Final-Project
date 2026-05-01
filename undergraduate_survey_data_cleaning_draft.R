########### LOADING DATASET ######################

library(readr)

Undergraduate_Student_Life_Survey_April_23_2026 <- read_csv("Undergraduate_Student_Life_Survey_April 23-2026.csv")
View(Undergraduate_Student_Life_Survey_April_23_2026)
undergrad_survey <- Undergraduate_Student_Life_Survey_April_23_2026

############ SUBSUBSETTING #########################


# Selecting needed variables "Finished" and responses for Q1 - Q16
v1_undergrad_survey_subset <- Undergraduate_Student_Life_Survey_April_23_2026[c(6,11:30)]


# Deleting rows from people whose "Finished" = False AND where Q1 (undergraduate 
# student status) = No or NA

library(dplyr)

v2_undergrad_survey_subset <- filter(v1_undergrad_survey_subset, Finished!="False" & Q1!= "No")

# deleting rows 1 and 2 (these rows are the question and importing information) both are unneeded

v3_undergrad_survey_subset <- v2_undergrad_survey_subset[-c(1,2), ] 




################ VARIABLE RECODING ###########################

library(tidyverse)

## Mutating observations "YES" and "NO" as 1 and 0
## Yes will now = 1 and No will = 0

# ***********************************************************************************
# the code below was written with the help of AI (ChatGPT)

# It runs the condition of if a cell contains "Yes" then it will be 
# changed to 1, if it is not "Yes" then the cell will be changed to 0 (NA is left unchanged)
# this condition is ONLY applied on columns where the WHOLE column contains ONLY observations "Yes", "No" or NA


v4_undergrad_survey_subset <- v3_undergrad_survey_subset %>%
  mutate(across(where(~ all(. %in% c("Yes", "No", NA))),
                ~ if_else(. == "Yes", 1, 0)))
# ***********************************************************************************
  
## Mutating Q2 (what year are you?)


# mutating "1 / 1st year" and so forth into simply 1,2,3,....

(v5_undergrad_survey_subset <- v4_undergrad_survey_subset %>%
  mutate(Q2 = case_when(
   
        Q2==  "1 / 1st year" ~ 1,
            
        Q2==   "2 / 2nd year" ~ 2,
            
        Q2==  "3 / 3rd year" ~ 3,
            
        Q2==   "4+ / 4th year and beyond" ~ 4
  )))



## Q3 (what is your major)


# observations will be assigned to a numerical value 1,2,3,4... based on how they appeared 
#to the respondants in the survey (e.g. Architecture = 1, Art=2, etc)




#data frame containing all majors in the order they appear in the survey

majors_order <- data.frame(majors= c("Architecture", "Art, Music, Dance, Theater","Buisness","Education",
             "Engineering", "Liberal Arts and Social Sciences","Natural Sciences and Mathematics"
             ,"Nursing", "Public Policy", "Integrated Studies"))


# **********************************************************************************
#this code was written with help from AI (ChatGPT)



#assigns an id to each of the majors in the df
majors_order$order_id <- seq_len(nrow(majors_order))


#joins the majors df and main df, replaces the character values with numbers, and replaces
# the old Q3 column with the "newly" coded column

v6_undergrad_survey_subset <- v5_undergrad_survey_subset %>%
  left_join(majors_order, by = c("Q3" = "majors")) %>%
  mutate(Q3 = order_id) %>%
  select(-order_id)



## Re-coding the Matrix (Q8)

#Order for the coding of frequencies in matrix

frequency_codes <- data.frame(frequency= c("Basically every day",
                                     "A few times a week",
                                     "A few times a month",
                                     "Once a month",
                                     "Less than once a month",
                                     "Once",
                                     "Not at all"))


#assigns numbers 0-6 to frequencies (1 for "not at all" and 7 for "basically everyday")

frequency_codes$order_id <- rev(seq_len(nrow(frequency_codes)))

library(stringr)

# the function below cleans the observations' format and ensures there are no 
# weird spaces and is treated as plain text (that is the purpose of the (as.character(.)) function )

v7_undergrad_survey_subset <- v6_undergrad_survey_subset %>%
  mutate(across(c(Q8_1, Q8_2, Q8_3, Q8_4, Q8_5, Q16),
                ~ str_squish(as.character(.))))

# does the same as above to the frequency codes df to ensure the formatting is exactly the same and will not produce 
# "NA"s at the last step

frequency_codes <- frequency_codes %>%
  mutate(frequency = str_squish(as.character(frequency)))

# turns my frequency_codes table into a named vector (a lookup dictionary)
# setNames makes R use the text as the key, and the number as the value

lookup <- setNames(frequency_codes$order_id, frequency_codes$frequency)

#For each value in each column:

#R will: take the string, use the string as a key in my "key" df, and then replace 
# the value in the df with the value assigned to that string across Q8_1-Q8_5, and Q16

v8_undergrad_survey_subset <- v7_undergrad_survey_subset %>%
  mutate(across(c(Q8_1, Q8_2, Q8_3, Q8_4, Q8_5, Q16),
                ~ lookup[.]))

# **********************************************************************************

## Q5 (only shown to those who responded "no" to Q4) why didnt you vote on campus

why_didnt_vote_codes <- data.frame(frequency= c("I chose not to vote",
                                         "I voted at a another polling location",
                                         "I did not know the election was taking place",
                                         "I could not get transportation to the polling place",
                                         "The line was too long",
                                         "I did not commute to campus that day",
                                         "I voted during Early Voting",
                                         "I do not feel strongly for politics/ voting"))



why_didnt_vote_codes$reason_id <- seq_len(nrow(why_didnt_vote_codes))














