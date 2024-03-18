######################## Plot - Employment Gains ###############################
# Aim:
# Plot (1): Overall Employment Gains
# Plot (2): Employment Gains by Location

rm(list=ls())


# Load packages ----------------------------------------------------------------
library(foreign)
library(dplyr)
library(stringr)
library(optmatch)
library(ggplot2)

# Load modeling results --------------------------------------------------------

# Employment one year arrival
load("output/employment_one_year/A.RData")
data <- A

# How often match
match_count <- sum(data$aid == data$raid)
match_count

# Plot 1) ----------------------------------------------------------------------
# Description:
# y-axis: federal states (aid)
# x- axis: predicted probability


# Actual assignment
data_actual <- data %>%
  group_by(aid) %>%
  summarise(employment_prob = mean(outcome))

# Algorithmic assignment
data_algo <- data %>%
  group_by(raid) %>%
  summarise(pred_employment_prob = mean(predprob)) %>%
  rename(aid=raid)

# Merge
data_merge <- data_algo %>%
  left_join(data_actual, by = "aid")

# Plot
library(tidyr)
df_long <- pivot_longer(data_merge, cols = c(pred_employment_prob, employment_prob), names_to = "Variable", values_to = "Value")

# Create the multibar plot
ggplot(df_long, aes(y = aid, x = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Value", y = "ID") +
  ggtitle("Multibar Plot for A and B") +
  theme_minimal()








