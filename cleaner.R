library(readxl)
library(tidyverse)

# Convert Excel data to csv

households <- read_excel("raw_data/UKR1904_R2_HCVA_HH.xlsx", 
                         sheet = "HoHH")
community_informants <- read_excel("raw_data/UKR1904_R2_HCVA_KI.xlsx", 
                                   sheet = "data")


# Create a dataset with services satisfaction variables as predictors and trust
# in government as an outcome

# Create lists of relevant variables to be able to iterate over them later

satisfaction_vars <- 
  c("f5_satisfaction_health", "d6_satisfaction_transport", "g4_satisfaction_admin", 
    "h2_satisfaction_social", "i2_fin_services_satisf", "j1_food_markets_satisf", 
    "j3_nfi_markets_satisf")
  
demographics_vars <- c("a2_sex", "a3_age")

# ^ might want to add more demographics variables, such as education level, 
# income level using one hot encoding (dummy variables)

# Select relevant variables

households_satisfaction <- households %>% 
  select(b44_1_trust_government, all_of(satisfaction_vars)) %>%
  filter(!is.na(b44_1_trust_government), b44_1_trust_government != "refuse")

# Convert categorical satisfaction/trust responses to a numeric scale (-2 to +2)
# for later use in regression

households_satisfaction <- households_satisfaction %>%
  
  # Outcome variable
  
  mutate(trust_government = case_when(
    b44_1_trust_government == "not_at_all" ~ -2,
    b44_1_trust_government == "little" ~ -1,
    b44_1_trust_government == "indifferent" ~ 0,
    b44_1_trust_government == "to_some_extent" ~ 1,
    b44_1_trust_government == "fully" ~ 2)) %>%

  # Predictor variables
  
  # health
  
  mutate(health_satisfaction = case_when(
    f5_satisfaction_health == "completely_dissatisfied" ~ -2,
    f5_satisfaction_health == "rather_disastisfied" ~ -1,
    f5_satisfaction_health == "indifferent" ~ 0,
    f5_satisfaction_health == "rather_satisfied" ~ 1,
    f5_satisfaction_health == "completely_satisfied" ~ 2)) %>%
  
  # transport
  
  mutate(transport_satisfaction = case_when(
    d6_satisfaction_transport == "completely_dissatisfied" ~ -2,
    d6_satisfaction_transport == "rather_disastisfied" ~ -1,
    d6_satisfaction_transport == "indifferent" ~ 0,
    d6_satisfaction_transport == "rather_satisfied" ~ 1,
    d6_satisfaction_transport == "completely_satisfied" ~ 2)) %>%
  
  # administrative
  
  mutate(admin_satisfaction = case_when(
    g4_satisfaction_admin == "completely_dissatisfied" ~ -2,
    g4_satisfaction_admin == "rather_disastisfied" ~ -1,
    g4_satisfaction_admin == "indifferent" ~ 0,
    g4_satisfaction_admin == "rather_satisfied" ~ 1,
    g4_satisfaction_admin == "completely_satisfied" ~ 2)) %>%
  
  # social
  
  mutate(social_satisfaction = case_when(
    h2_satisfaction_social == "completely_dissatisfied" ~ -2,
    h2_satisfaction_social == "rather_disastisfied" ~ -1,
    h2_satisfaction_social == "indifferent" ~ 0,
    h2_satisfaction_social == "rather_satisfied" ~ 1,
    h2_satisfaction_social == "completely_satisfied" ~ 2)) %>% 
  
  # financial
  
  mutate(financial_satisfaction = case_when(
    i2_fin_services_satisf == "completely_dissatisfied" ~ -2,
    i2_fin_services_satisf == "rather_disastisfied" ~ -1,
    i2_fin_services_satisf == "indifferent" ~ 0,
    i2_fin_services_satisf == "rather_satisfied" ~ 1,
    i2_fin_services_satisf == "completely_satisfied" ~ 2)) %>%
  
  # food markets
  
  mutate(food_markets_satisfaction = case_when(
    j1_food_markets_satisf == "completely_dissatisfied" ~ -2,
    j1_food_markets_satisf == "rather_disastisfied" ~ -1,
    j1_food_markets_satisf == "indifferent" ~ 0,
    j1_food_markets_satisf == "rather_satisfied" ~ 1,
    j1_food_markets_satisf == "completely_satisfied" ~ 2)) %>%
  
  # non food item markets
  
  mutate(non_food_markets_satisfaction = case_when(
    j3_nfi_markets_satisf == "completely_dissatisfied" ~ -2,
    j3_nfi_markets_satisf == "rather_disastisfied" ~ -1,
    j3_nfi_markets_satisf == "indifferent" ~ 0,
    j3_nfi_markets_satisf == "rather_satisfied" ~ 1,
    j3_nfi_markets_satisf == "completely_satisfied" ~ 2)) %>%
  
  # Leave only numeric-converted variables
  
  select(trust_government:non_food_markets_satisfaction)


# Write  datasets into csv

write.csv(households, "ukraine_frontline/data/households.csv")
write.csv(households_satisfaction, "ukraine_frontline/data/households_satisfaction.csv")
write.csv(community_informants, "ukraine_frontline/data/community_informants.csv")



# Aggregate the households dataset by hromada and save as a new dataset

# households_aggregated <- households %>%
#   filter(hromada != "NA") %>%
#   group_by(hromada)




# 
# summarize(n_households = n(),
#           hohh_employment_status.full_time = mean(b5_hohh_employment_status.full_time),
#           .groups = "drop") 
# 
# var_name <- satisfaction_vars[1]
# 
# for (i in 1:length(satisfaction_vars)){
#   households_satisfaction <<- households_satisfaction %>%
#     get(satisfaction_vars[i])
# }