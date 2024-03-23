# installing packages
install.packages("pacman")
pacman::p_load(
  rio,         # importing data  
  here,        # relative file pathways  
  tidyverse,   # data management and visualization
  magrittr,
  FactoMineR,  # FAMD
  factoextra   # FAMD
)

# importing datasets
fight_raw <- import("ufc_data_till_UFC_292.csv")
fighter_raw <- import("ufc-fighters-statistics.csv")

View(fight_raw)
View(fighter_raw)
names(fight_raw)
names(fighter_raw)

fighter <- fighter_raw %>%
  select(name, date_of_birth)

# FAMD
library(FactoMineR)
fight_raw %>%
  FAMD(ncp = 10, graph = TRUE) %>%
  print()
