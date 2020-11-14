library(readxl)

households_round2_data <- read_excel("raw_data/UKR1904_R2_HCVA_HH.xlsx", 
                                     sheet = "HoHH")
key_informants_round2_data <- read_excel("raw_data/UKR1904_R2_HCVA_KI.xlsx", 
                                         sheet = "data")
multi_sectoral_needs_assessment_data <- read_excel("raw_data/REACH_UKR_Dataset_MSNA_2020.xlsx", 
                                                   sheet = "dataset")

write.csv(households_round2_data, "ukraine_frontline/data/HH_R2.csv")
write.csv(key_informants_round2_data, "ukraine_frontline/data/KI_R2.csv")
write.csv(multi_sectoral_needs_assessment_data, "ukraine_frontline/data/MSNA.csv")

