# Load relevant libraries

library(tidyverse)
library(ggthemes)

# Use the model from a different R script

source("model.R")

# Set the default ggplot theme and scale 

ggplot2::theme_set(ggplot2::theme_minimal(base_size = 13))

# 1. Create a graphic to represent what percentage of households reported 
# their satisfaction with various services, out of those who did report their 
# level of trust in local government

# Create a tibble with proportions for each service category

reported_satisfaction <- 
  tibble(
    health = mean(!is.na(households_satisfaction$health_satisfaction)),
    transport = mean(!is.na(households_satisfaction$transport_satisfaction)),
    admin = mean(!is.na(households_satisfaction$admin_satisfaction)),
    social = mean(!is.na(households_satisfaction$social_satisfaction)),
    financial = mean(!is.na(households_satisfaction$financial_satisfaction)),
    food_markets = mean(!is.na(households_satisfaction$food_markets_satisfaction)),
    non_food_markets = mean(!is.na(households_satisfaction$non_food_markets_satisfaction))
    ) %>% 
  
  # Pivot longer to be able to plot proportions on one graphic
  
  pivot_longer(cols = health:non_food_markets, 
               names_to = "category",
               values_to = "proportion_households")

# Create a graphic

services_proportion_plot <- reported_satisfaction %>%
  ggplot(aes(proportion_households, 
             fct_reorder(category, proportion_households),
             fill = category)) +
  geom_col() +
  scale_y_discrete(breaks = c("health", "transport", "admin", "social", 
                              "financial", "food_markets", "non_food_markets"),
                   labels = c("health", "public transport", "administrative", 
                              "social", "financial", "food markets", 
                              "non-food markets")) +
  scale_x_continuous(labels = scales::percent_format()) +
  
  # Set custom fill colors to make the Shiny page more aesthetically pleasing
  
  scale_fill_manual(breaks = c("health", "transport", "admin", "social", 
                               "financial", "food_markets", "non_food_markets"),
                    values = c("#ff0039", 
                               "#9a53bb",
                               "#ffa600",
                               "#383a3c",
                               "#ff7518",
                               "#3fb719",
                               "#2780e3")) +
  
  labs(title = "Households' Use of Services",
       subtitle = "Proportion of households that reported having used the 
following services in the last 3 months, out of those who 
also reported their level of trust in local government",
       x = "Proportion of Households",
       y = "Category of Services") +
  
  # Remove legend since the categories are indicated on the y axis already and 
  # fill colors are only added for aesthetics
  
  theme(legend.position = "none")


# 2. Create a tibble for the plot of posterior distributions of parameters 
# from the multiple linear regression, later used in the server in app.R

posteriors_tibble <- fit_government_trust %>%
  as_tibble() %>%
  select(- c(sigma, `(Intercept)`)) %>%
  mutate(health = health_satisfaction, 
         transport = transport_satisfaction,
         admin = admin_satisfaction,
         social = social_satisfaction,
         financial = financial_satisfaction,
         food_markets = food_markets_satisfaction,
         non_food_markets = non_food_markets_satisfaction) %>%
  select(health:non_food_markets) %>%
  pivot_longer(cols = health:non_food_markets,
               names_to = "parameter",
               values_to = "value") 
