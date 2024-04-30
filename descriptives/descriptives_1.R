############################ Descriptives - Lframe ######################################
# Aim: run some descriptives

rm(list=ls())


# Load packages ----------------------------------------------------------------
library(foreign)
library(dplyr)
library(stringr)
library(ggplot2)
library(UpSetR)
library(naniar)



# Load data --------------------------------------------------------------------
# Lframe - 9.368
load("data/processed/Lframe.RData")

# Change format ----------------------------------------------------------------
Lframe$immiyear <- factor(Lframe$immiyear)
Lframe$corigin <- factor(Lframe$corigin)
Lframe$sex <- factor(Lframe$sex)
Lframe$bula_res <- factor(Lframe$bula_res)
Lframe$refugee_sample <- factor(Lframe$refugee_sample)


# Overview ---------------------------------------------------------------------


# Number of unique observations
length(unique(Lframe$rid))

# Number of missings per column
gg_miss_upset(Lframe, nsets = n_var_miss(Lframe))

# Distribution observations across immigration years
ggplot(Lframe, aes(x = immiyear)) +
  geom_bar()+
  labs(x = "Immigration Year", y = "Number of Observations", fill = "Country of Origin") +
  ggtitle("Number of Observations by Immigration Year and Country of Origin") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# Distribution observations across immigration years and country of origin
ggplot(Lframe, aes(x = immiyear, fill = corigin)) +
  geom_bar(position = "stack") +
  labs(x = "Immigration Year", y = "Number of Observations", fill = "Country of Origin") +
  ggtitle("Number of Observations by Immigration Year and Country of Origin") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Distribution observations across immigration years and gender
table(Lframe$immiyear, Lframe$sex)

ggplot(Lframe, aes(x = immiyear, fill = sex)) +
  geom_bar(position = "stack") +
  labs(x = "Immigration Year", y = "Number of Observations", fill = "Sex") +
  ggtitle("Number of Observations by Immigration Year and Sex") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Distribution observations across immigration years and assigned federal state
table(Lframe$immiyear, Lframe$bula_res)

ggplot(Lframe, aes(x = immiyear, fill = bula_res)) +
  geom_bar(position = "stack") +
  labs(x = "Immigration Year", y = "Number of Observations", fill = "Assigned Federal State") +
  ggtitle("Number of Observations by Immigration Year and Assigned Federal State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Distribution observations across immigration years and refugee/mig sample
table(Lframe$immiyear, Lframe$refugee_sample)

ggplot(Lframe, aes(x = immiyear, fill = refugee_sample)) +
  geom_bar(position = "stack") +
  labs(x = "Immigration Year", y = "Number of Observations", fill = "Refugee Sample") +
  ggtitle("Number of Observations by Immigration Year and Refugee Sample") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))












