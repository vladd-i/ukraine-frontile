# Load relevant libraries

library(rstanarm)
library(broom.mixed)
library(gt)
library(gtsummary)
library(tidyverse)

# Read in data

households_satisfaction <- read.csv("data/households_satisfaction.csv")
  
# Alter the dataset before using it to fit a model

# Impute 0 ("Indifferent") for missing satisfaction levels (reasoning behind
# this choice is explained in the "Discussion" page of the Shiny app)

households_satisfaction_imputed <- households_satisfaction

households_satisfaction_imputed[is.na(households_satisfaction_imputed)] <- 0

# Create a Bayesian linear model to predict trust in local government from 
# satisfaction with key services

set.seed(2014)

fit_government_trust <- stan_glm(
  trust_government ~ health_satisfaction + transport_satisfaction +
    admin_satisfaction + social_satisfaction + financial_satisfaction +
    food_markets_satisfaction + non_food_markets_satisfaction,
  data = households_satisfaction_imputed,
  refresh = 0)

# Put regression results into a table

gt_tbl <- fit_government_trust %>%
  tbl_regression(intercept = TRUE) %>%
  as_gt() %>%
  tab_header(title = "Regression of Trust in Local Government",
             subtitle = "The effects of satisfaction with key services on the level
             of trust in local government to take care of its citizens") %>%
  tab_source_note(md("Source: AGORA Initiative"))
