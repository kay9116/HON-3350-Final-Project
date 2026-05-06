######### Packges and Loading datasets #####################

library(ggplot2)
library(dplyr)


install.packages('ggimage')
library(ggimage)


########### Q2 & 3 ##########################


# code used to get the information to create canva pictograph for Q2 and 3

table(undergrad_survey$Q2)
table(undergrad_survey$Q3)

############ Q4 ################################

#counts for variable Q4

undergrad_survey %>%
  count(Q4)


summary(undergrad_survey$Q4)

# **********************************************************************************
# the script below was created with help from AI


#creates the df necessary for the plot creation

q4_clean <- undergrad_survey %>%
  mutate(voted_group = case_when(
    Q4 == 1 ~ "Voted On-Campus",
    Q4 == 0 ~ "Did Not Vote On-Campus",
    is.na(Q4) ~ "NA"
  ))

q4_plot_data <- q4_clean %>%
  count(voted_group) %>%
  mutate(percent = n / sum(n) * 100)

q4_plot_data <- q4_plot_data %>%
  mutate(voted_group = factor(voted_group, levels = c(
    "Did Not Vote On-Campus",
    "Voted On-Campus",
    "NA"
  )))


#creates the split bar graph
ggplot(q4_plot_data, aes(x = "All Students", y = percent, fill = voted_group)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_stack(vjust = 0.85),
    color = "white",
    size = 3
  ) +
  scale_fill_manual(values = c(
    "Voted On-Campus" = "firebrick",
    "Did Not Vote On-Campus" = "gray70",
    "NA" = "black"
  )) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"))+
  labs(
    title = "Majority of Students Did Not Vote On-Campus",
    subtitle = "Only 31.7 % of of the respondants voted on- campus for the March Primaries ",
    x = NULL,
    y = "Percentage",
    fill = "Group"
  ) +
  coord_flip() +
  theme_minimal()


################## Q5 #########################################

read_csv(Q5_didnt_vote_reason_df)


# creates a count df for Q5

Q5_plot_data <- Q5_didnt_vote_reason_df %>%
  count(Q5) %>%
  mutate(percent = n / sum(n) * 100)


ggplot(Q5_plot_data, aes(x = Q5, y = percent)) +
  geom_col(fill = "firebrick", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Reasons Students Do Not Vote On Campus (by Percent)",
    subtitle = "Most students reported voting at another polling location or during early voting",
    x = NULL,
    y = "Percentage"
  ) +
  coord_flip() +
  theme_minimal()


ggplot(Q4_plot_data, aes(x = "All Students", y = percent, fill = voted)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_stack(vjust = 0.75),
    color = "white",
    size = 3
  ) +
  scale_fill_manual(values = c(
    "Voted" = "firebrick",
    "Did Not Vote" = "gray70"
  )) +
  labs(
    title = "Majority of Students do not Vote on Campus for Elections",
    subtitle = " Only 32.5% of respondants surveyed reported voting on campus for the March Primaries",
    x = NULL,
    y = "Percentage",
    fill = "Response"
  ) +
  theme_minimal()+
  coord_flip()

############# Q5 and Q4  #########


#removes doubles to ensure respondants are not being double counted
no_doubles_Q5_didnt_vote_reason_df <-  Q5_didnt_vote_reason_df %>%
  distinct(ResponseId, .keep_all = TRUE)



# assigns vote 1,0

no_doubles_Q5_didnt_vote_reason_df <- no_doubles_Q5_didnt_vote_reason_df %>%
  mutate(voted = if_else(`Q5 ID` %in% c(2, 7), 1, 0))

table(no_doubles_Q5_didnt_vote_reason_df$voted)

#creates the split bar graph adding in voted off campus

merged_df <- undergrad_survey %>%
  left_join(
    no_doubles_Q5_didnt_vote_reason_df %>%
      select(ResponseId, voted),
    by = "ResponseId"
  )

merged_df %>%
  mutate(group = case_when(
    Q4 == 1 ~ "Voted on-campus",
    Q4 == 0 & voted == 1 ~ "Voted off-campus",
    Q4 == 0 & voted == 0 ~ "Did not vote",
    TRUE ~ "NA"
  ))

split_data <- merged_df %>%
  mutate(group = case_when(
    Q4 == 1 ~ "Voted on-campus",
    Q4 == 0 & voted == 1 ~ "Voted off-campus",
    Q4 == 0 & voted == 0 ~ "Did not vote",
    TRUE ~ "NA"
  )) %>%
  count(group) %>%
  mutate(percent = n / sum(n) * 100)


split_data <- split_data %>%
  mutate(group = factor(group, levels = c(
    "Did not vote",
    "Voted off-campus",
    "Voted on-campus",
    "NA"
  )))


ggplot(split_data, aes(x = "", y = percent, fill = group)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_stack(vjust = 0.9),
    color = "white",
    size = 3
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c(
    "Did not vote" = "gray60",
    "Voted off-campus" = "red3",
    "Voted on-campus" = "firebrick",
    "NA" = "black"
  )) +
  labs(
    title = "Student Voting Participation Breakdown",
    subtitle = "On-campus vs off-campus vs non-voters",
    x = NULL,
    y = "Percentage",
    fill = "Group"
  ) +
  coord_flip() +
  theme_minimal()


# ***********************************************************************************************************



############## Q8 Matrix  ################################################


summary(undergrad_survey$Q8_5)















































