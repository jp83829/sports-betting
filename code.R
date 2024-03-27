# install packages
install.packages("pacman")
pacman::p_load(
  rio,         # importing data  
  here,        # relative file pathways  
  tidyverse,   # data management and visualization
  magrittr,
  FactoMineR,  # FAMD
  factoextra  # FAMD
)

# import datasets
fight_raw <- import("ufc_data_till_UFC_292.csv")
fighter_raw <- import("ufc-fighters-statistics.csv")

View(fight_raw)
View(fighter_raw)
names(fight_raw)
names(fighter_raw)

fighter <- fighter_raw %>%
  select(name, date_of_birth)

# standardize variables
count <- c('R_SIG_STR.', 'B_SIG_STR.', 'R_TOTAL_STR.', 'B_TOTAL_STR.', 'R_TD', 
           'B_TD', "R_HEAD", "B_HEAD", "R_BODY", "B_BODY", "R_DISTANCE", "B_DISTANCE") #character "x of y" to be converted to percentage
time <- c('R_CTRL', 'B_CTRL') #character time span to be converted to numeric
fight <- fight_raw %>% 
  mutate_at(vars(count), ~ as.numeric(str_split_i(., " of ", 1))/as.numeric(str_split_i(., " of ", 2)) ) %>%
  mutate_at(vars(time), ~ as.numeric(str_split_i(., ":", 1))+as.numeric(str_split_i(., ":", 2))/60) %>%
  mutate_all(~ifelse(is.nan(.), 0, .)) %>%
  mutate_all(~ifelse(is.na(.), 0, .))

View(fight)
skim(fight)


# sample test
set.seed(2024)
part_df <- fight %>% 
  slice_sample(n=100)

# FAMD
exc <- c('R_SIG_STR_pct', 'B_SIG_STR_pct', 'R_TD_pct', 'B_TD_pct', 'R_REV', 
         'B_REV', 'last_round_time', 'R_fighter', 'B_fighter', "Format",
         "Referee", "date", "location", 'Winner', 'R_TD', 'B_TD', "R_CLINCH", 
         "B_CLINCH", "R_GROUND", "B_GROUND", "R_LEG", "B_LEG") # supplementary variable, which is not included in the analysis
res.famd <- part_df %>%
  FAMD(ncp = 10, 
       sup.var = var(exc),  
       graph = TRUE)

fviz_screeplot(res.famd)

