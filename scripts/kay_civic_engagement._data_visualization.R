######### Packges and Loading datasets #####################

library(readr)
combined_cleaned <- read_csv("data/cleaned/combined_cleaned.csv")
View(combined_cleaned)

library(ggplot2)
library(dplyr)



########### Q2 & 3 ##########################


# code used to get the information to create canva pictograph for Q2 and 3

table(combined_cleaned$Q2)
table(combined_cleaned$Q3)

############ Q4 ################################

#counts for variable Q4

combined_cleaned %>%
  count(Q4)


summary(combined_cleaned$Q4)

# **********************************************************************************
# the script below was created with help from AI


#creates the df necessary for the plot creation

q4_clean <- combined_cleaned %>%
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

Q5_didnt_vote_reason_df <- read_csv("data/cleaned/Q5_reason_didnt_vote_df")


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



############# Q5 and Q4  #########


#removes doubles to ensure respondants are not being double counted
no_doubles_Q5_didnt_vote_reason_df <-  Q5_didnt_vote_reason_df %>%
  distinct(ResponseId, .keep_all = TRUE)



# assigns vote 1,0

no_doubles_Q5_didnt_vote_reason_df <- no_doubles_Q5_didnt_vote_reason_df %>%
  mutate(voted = if_else(`Q5 ID` %in% c(2, 7), 1, 0))

table(no_doubles_Q5_didnt_vote_reason_df$voted)

#creates the split bar graph adding in voted off campus

merged_df <- combined_cleaned %>%
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






############## Q8 Matrix  ################################################


# creating an index from Q8.1-5 to create a civic engagement score for students

install.packages("psych")

library(psych)
library(ggplot2)

# Subsetting the the dataset
Q8_index_df <- combined_cleaned[, c("Q8_1", "Q8_2", "Q8_3", "Q8_4","Q8_5")]

# Run PCA
pca_result <- principal(
  Q8_index_df,
  nfactors = 5,
  rotate = "none",
  scores = TRUE
)

# View full output
pca_result


## complies the info from pca_result into a nice table

install.packages("gt")
library(gt)
library(tibble)

pca_table <- as.data.frame(unclass(pca_result$loadings)) %>%
  round(2) %>%
  rownames_to_column("Variable")

pca_table %>%
  gt() %>%
  tab_header(
    title = "Principal Component Loadings"
  )

## creates a variance table

variance_table <- data.frame(
  Metric = c("SS Loadings", "Proportion Variance", "Cumulative Variance"),
  PC1 = c(
    pca_result$Vaccounted["SS loadings", 1],
    pca_result$Vaccounted["Proportion Var", 1],
    pca_result$Vaccounted["Cumulative Var", 1]
  ),
  PC2 = c(
    pca_result$Vaccounted["SS loadings", 2],
    pca_result$Vaccounted["Proportion Var", 2],
    pca_result$Vaccounted["Cumulative Var", 2]
  ),
  PC3 = c(
    pca_result$Vaccounted["SS loadings", 3],
    pca_result$Vaccounted["Proportion Var", 3],
    pca_result$Vaccounted["Cumulative Var", 3]
  ),
  PC4 = c(
    pca_result$Vaccounted["SS loadings", 4],
    pca_result$Vaccounted["Proportion Var", 4],
    pca_result$Vaccounted["Cumulative Var", 4]
  ),
  PC5 = c(
    pca_result$Vaccounted["SS loadings", 5],
    pca_result$Vaccounted["Proportion Var", 5],
    pca_result$Vaccounted["Cumulative Var", 5]
  )
  
)

## rounds the values in the variance table to the nearest thousandth
variance_table <- variance_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# creates the table
variance_table %>%
  gt() %>%
  tab_header(
    title = "PCA Variance Explained"
  )


# results indicate each person should get one score
pca_result <- principal(Q8_index_df, nfactors = 1, rotate = "none")

scree(Q8_index_df, factors = FALSE)

# opens the plot generated in a seperate window
windows()
fa.parallel(Q8_index_df, fa = "pc") #creates a scree plot


#creates the actual index

Q8_index_df$Q8_index <- pca_result$scores[,1]


# creates histogram of scores
hist(Q8_index_df$Q8_index)

ggplot(Q8_index_df, aes(x = Q8_index)) +
  geom_histogram(binwidth= .5, fill = "steelblue", color = "white") +
  theme_minimal()+
  labs(title = "Q8 Civic Engagement Score Distribution",
       x = " Civic Engagement Score",
       y = "Frequency")

# ***********************************************************************************************************

write_csv(Q8_index_df, "Q8_Index.csv")




























