library(tidyverse)
library(ggthemes)

# Use the model from a different R script

source("model.R")

# Set the default ggplot theme 

ggplot2::theme_set(ggplot2::theme_minimal(base_size = 13))

# 1. Create a graphic to represent what percentage of households reported 
# their satisfaction with various services, out of those who did report their 
# level of trust in local government

# Create a tibble with proportions for each service category

reported_satisfaction <- 
  tibble(health = mean(!is.na(households_satisfaction$health_satisfaction)),
         transport = mean(!is.na(households_satisfaction$transport_satisfaction)),
         admin = mean(!is.na(households_satisfaction$admin_satisfaction)),
         social = mean(!is.na(households_satisfaction$social_satisfaction)),
         financial = mean(!is.na(households_satisfaction$financial_satisfaction)),
         food_markets = mean(!is.na(households_satisfaction$food_markets_satisfaction)),
         non_food_markets = mean(!is.na(households_satisfaction$non_food_markets_satisfaction))) %>% 
  
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
  
  # Create custom fill color to make the Shiny page more aesthetically pleasing
  
  scale_fill_manual(breaks = c("health", "transport", "admin", "social", 
                               "financial", "food_markets", "non_food_markets"),
                    values = c("#ff0039", 
                               "#9a53bb",
                               "#ffa600",
                               "#383a3c",
                               "#ff7518",
                               "#3fb719",
                               "#2780e3")) +
  
  labs(title = "Households' Usage of Services",
       subtitle = "Proportion of households that reported having used the 
following services in the last 3 months, out of those who 
also reported their level of trust in local government",
       x = "Proportion of Households",
       y = "Category of Services") +
  
  # Remove legend since the categories are indicated on the y axis
  
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



# 
# 
# 
# fit_government_trust %>%
#   as_tibble() %>%
#   select(- c(sigma, `(Intercept)`)) %>%
#   mutate(health = health_satisfaction,
#          transport = transport_satisfaction,
#          admin = admin_satisfaction,
#          social = social_satisfaction,
#          financial = financial_satisfaction,
#          food_markets = food_markets_satisfaction,
#          non_food_markets = non_food_markets_satisfaction) %>%
#   select(health:non_food_markets) %>%
# 
#   pivot_longer(cols = health:non_food_markets,
#                names_to = "parameter",
#                values_to = "value") %>%
# 
#   ggplot(aes(value, fill = parameter)) +
# 
#   geom_histogram(aes(y = after_stat(count/sum(count))),
#                  alpha = 0.7,
#                  bins = 500,
#                  position = "identity") +
# 
#     # Density curves create a less crowded graphic than histograms:
#   geom_density(aes(y = after_stat(count/sum(count))),
#                alpha = .8) +
# 
#   scale_fill_manual(breaks = c("social", "admin", "health", "transport",
#                                "food_markets", "financial", "non_food_markets"),
#                     values = c("#383a3c",
#                                "#ffa600",
#                                "#ff0039",
#                                "#9a53bb",
#                                "#3fb719",
#                                "#ff7518",
#                                "#2780e3"),
#                     labels = c("social", "administrative", "health",
#                                "public transport", "food markets", "financial",
#                                "non-food markets"),
#                     name = "Satisfaction with\na service category") +
# 
#   # Set x and y limits so that when variables are added/removed, the scale 
#   # stays the same
#   
#   xlim(-.1, .3) + 
#   ylim(0, .38) +
# 
#   labs(title = "Posterior Probability Distributions",
#        subtitle = "Relative strengths of predictors of households'\ntrust in local government",
#        x = "Coefficient value",
#        y = "Probability") +
#   scale_y_continuous(labels = scales::percent_format())
