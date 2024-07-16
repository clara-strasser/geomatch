######################## Plot - Employment Gains ###############################
# Aim:
# Plot (1): Employment Gains by Location
# Plot (2): Overall Employment Gains

rm(list=ls())


# Load packages ----------------------------------------------------------------
library(foreign)
library(dplyr)
library(stringr)
library(optmatch)
library(ggplot2)
library(tidyr)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser"
path_data_soep <- file.path(base_path, "soep_data", "final")
path_data_geomatch <- file.path(base_path, "geomatch_data", "final")
path_data_final <- file.path(base_path, "geomatch_data")

# Load modeling results --------------------------------------------------------

# Employment two year arrival
load(paste0(path_data_final, "/refmig_ref/", "employment_two_year_arrival/A.RData"))
data_1 <- A

# Employment three year arrival
load(paste0(path_data_final, "/refmig_ref/", "employment_three_year_arrival/A.RData"))
data_2 <- A

# Employment four year arrival
load(paste0(path_data_final, "/refmig_ref/", "employment_four_year_arrival/A.RData"))
data_3 <- A

# Change data types ------------------------------------------------------------

data <- data %>% 
  mutate(aid = as.integer(aid),
         raid = as.integer(raid))

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

# Modify
data_long <- pivot_longer(data_merge, cols = c(pred_employment_prob, employment_prob), names_to = "Variable", values_to = "Value")

# Re-level
custom_labels <- c("Thuringia", "Saxony-Anhalt", "Saxony", 
                   "Mecklenburg-Western Pomerania", "Brandenburg", "Berlin",
                   "Saarland", "Bavaria", 
                   "Baden-Wuerttemberg", "Rhineland-Palatinate", "Hesse",
                   "North Rhine-Westphalia", "Bremen", "Lower Saxony", 
                   "Hamburg", "Schleswig-Holstein")

data_long$aid <- factor(data_long$aid, levels = 16:1, labels = custom_labels)



# Multibar Plot
plot <- ggplot(data_long, aes(y = aid, x = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Average Employment Probability", y = "Federal State", title = "Employment One Year After Arrival") +
  ggtitle("") +
  theme_minimal() +
  theme(#panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.key.size = unit(18, "points"),
    legend.position = c(0.95, 0.95),  
    legend.justification = c(1,1),
    legend.box.just = "right") +
  scale_fill_manual(labels = c("Actual", "Algorithmic"), values = c("#66A182","#2E4057")) +
  guides(fill = guide_legend(title = "Location Assignment")) +
  xlim(0, 1) 

# Save
ggsave(plot = plot, filename = "employment_prob_second_year.png", path = "output/plots/", width = 15, height = 8) 
ggsave(plot = plot, filename = "employment_prob_third_year.png", path =paste0(path_data_final, "/output/plots/"), width = 15, height = 8) 



# Plot 2) ----------------------------------------------------------------------
# Description:
# y - axis: Average Employment Probability
# x - axis: Labels (Employment Second Year Arrival vs Employment Third Year Arrival)

# Create data
data_comparison <- data.frame(
  outcome = c("Two", "Three", "Four"),
  actual = c(mean(data_1$outcome), mean(data_2$outcome), mean(data_3$outcome)),
  alg = c(mean(data_1$predprob), mean(data_2$predprob),  mean(data_3$predprob))
)


# Modify
data_long <- pivot_longer(data_comparison, cols = c(actual, alg ), names_to = "Variable", values_to = "Value")

data_long$outcome <- factor(data_long$outcome, levels = c("Two", "Three", "Four"))

# Plot
plot_dif <- 
  ggplot(data_long, aes(x = factor(outcome), y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(x = "Year(s) after Arrival", y = "Average Employment Probability") +
  theme_minimal() +
  theme(#panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    legend.key.size = unit(18, "points"),
    legend.position = "top",  
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.box.just = "right",
    aspect.ratio = 1.2/1) +
  scale_fill_manual(labels = c("Actual", "Algorithmic"), values = c("#66A182","#2E4057", "#33423D")) +
  guides(fill = guide_legend(title = "Location Assignment:")) +
  ylim(0, 0.4)
plot_dif

plottest <- ggarrange(plot_dif2, plot_dif, ncol = 2)


plottest <- ggarrange(defplot20, defplot21)
ggsave(plot = plot_dif, filename = "employment_prob_dif_refmig_ref.png", path =paste0(path_data_final, "/output/plots/"), width = 15, height = 8) 



# Plot 3) ----------------------------------------------------------------------
# Description:
# y - axis: Average Employment Probability
# x - axis: Labels Train "Refugees" and "Refugees and Migrants"

# Create data
data_comparison <- data.frame(
  outcome = c("One", "Two"),
  actual = c(mean(data_1$outcome), mean(data_2$outcome)),
  alg = c(mean(data_1$predprob), mean(data_2$predprob))
)


# Modify
data_long <- pivot_longer(data_comparison, cols = c(actual, alg ), names_to = "Variable", values_to = "Value")

# Plot
plot_dif <- 
  ggplot(data_long, aes(x = factor(outcome), y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(x = "Year(s) after Arrival", y = "Average Employment Probability") +
  theme_minimal() +
  theme(#panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    legend.key.size = unit(18, "points"),
    legend.position = "top",  
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.box.just = "right",
    aspect.ratio = 1.2/1) +
  scale_fill_manual(labels = c("Actual", "Algorithmic"), values = c("#66A182","#2E4057")) +
  guides(fill = guide_legend(title = "Location Assignment:")) 
plot_dif


ggsave(plot = plot_dif, filename = "employment_prob_dif.png", path =paste0(path_data_final, "/output/plots/"), width = 15, height = 8) 























