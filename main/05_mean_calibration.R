######################## Model Performance ####################################
# Mean Calibration

rm(list=ls())


# Load packages ----------------------------------------------------------------

library(predtools)
library(magrittr)
library(dplyr)
library(ggplot2)



# 3) Modify data --------------------
data <- LRtoOMout[[1]]

# 3) Modify data --------------------
data <- data %>%
  select(-1:-((ncol(data)-18))) 
data <- data %>%
  rename_with(~paste0("pred_", .), matches("^\\d+$"))

# 4) Generate overall PRAUC value -----------
data$pred <- apply(data, 1, function(x) x[paste0("pred_", x["aid"])])
data <- data %>%
  mutate(pred = as.numeric(pred)) 


plot_list  <- calibration_plot(data = data, obs = "outcome", pred = "pred")
plot <- plot_list$plot 
ggsave("output/plots/calibration/1516_refmig_refmig.png", plot)







