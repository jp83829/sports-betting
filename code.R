# install packages
install.packages("pacman")
pacman::p_load(
  rio,         # importing data  
  here,        # relative file pathways  
  tidyverse,   # data management and visualization
  magrittr,
  FactoMineR,  # FAMD
  factoextra   # FAMD
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
pct <- c('R_SIG_STR_pct', 'B_SIG_STR_pct') #character percentage to be converted to numeric 
fight <- fight_raw %>% 
  mutate_at(vars(pct), ~ str_replace(., "%", "")) %>%
  mutate_at(vars(pct), as.numeric) %>%
  mutate(R_TOTAL_STR_pct=str_split(R_TOTAL_STR., ' of ', n = 2, simplify = T) %>% as.numeric() ) #character "x of y" to be converted to percentage 




count <- c('R_TOTAL_STR.', 'B_TOTAL_STR.')

# sample test
set.seed(2024)
part_df <- fight %>% 
  slice_sample(n=100)

# FAMD
exc <- c('R_SIG_STR.', 'B_SIG_STR.', 'R_TOTAL_STR.') # supplementary variable, which is not included in the analysis
res.famd <- part_df %>%
  FAMD(ncp = 10, 
       sup.var = var(exc),  
       graph = TRUE)

fviz_screeplot(res.famd)

