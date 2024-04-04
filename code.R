# install packages
install.packages("pacman")
pacman::p_load(
  rio,         # importing data  
  here,        # relative file pathways  
  tidyverse,   # data management and visualization
  magrittr,
  FactoMineR,  # FAMD
  factoextra,  # FAMD
  skimr,
  GGally,
  eeptools 
  )

# import datasets
fight_raw <- import("ufc_data_till_UFC_292.csv")
fighter_raw <- import("ufc-fighters-statistics.csv")

age_calc_miss <- function(dob, enddate = Sys.Date(), units = "months", precise = TRUE){
  retval <- rep(NA_real_, length(dob))
  miss <- is.na(dob)
  retval[!miss] <- eeptools::age_calc(dob = dob[!miss], 
                                      enddate = enddate, 
                                      units = units, 
                                      precise = precise)
  retval
  }

fighter <- fighter_raw %>%
  select(name, height_cm, reach_in_cm, stance, date_of_birth) %>%
  mutate(age = floor(age_calc_miss(date_of_birth, units = "years"))) %>%
  subset(! name %in% c(name[duplicated(name)]))

# standardize variables
count <- c('R_SIG_STR.', 'B_SIG_STR.', 'R_TOTAL_STR.', 'B_TOTAL_STR.', 'R_TD', 
           'B_TD', "R_HEAD", "B_HEAD", "R_BODY", "B_BODY", "R_DISTANCE", "B_DISTANCE") #character "x of y" to be converted to percentage
time <- c('R_CTRL', 'B_CTRL') #character time span to be converted to numeric
fight <- fight_raw %>% 
  mutate_at(vars(count), ~ as.numeric(str_split_i(., " of ", 1))/as.numeric(str_split_i(., " of ", 2)) ) %>%
  mutate_at(vars(time), ~ as.numeric(str_split_i(., ":", 1))+as.numeric(str_split_i(., ":", 2))/60) %>%
  mutate_all(~ifelse(is.nan(.), 0, .)) %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%
  subset(Winner!="") %>%
  left_join(fighter,
    by = c("R_fighter" = "name"),
    suffix = c(".x", ".y"),
    keep = NULL)
?left_join



View(fight)
skim(fight)

num <- c("R_KD", "B_KD", "R_SIG_STR.", "B_SIG_STR.", "R_TOTAL_STR.", "B_TOTAL_STR.", 
         "R_SUB_ATT", "B_SUB_ATT", "R_CTRL", "B_CTRL", "R_HEAD", "B_HEAD", 
         "R_BODY", "B_BODY", "R_DISTANCE", "B_DISTANCE", "win_by", "last_round")
fight[, num] %>% 
  ggpairs()

# sample test
set.seed(2024)
part_df <- fight %>% 
  slice_sample(n=100)

# FAMD
exc <- c('R_SIG_STR_pct', 'B_SIG_STR_pct', 'R_TD_pct', 'B_TD_pct', 'R_REV', 
         'B_REV', 'last_round_time', 'R_fighter', 'B_fighter', "Format",
         "Referee", "date", "location", 'Winner', 'R_TD', 'B_TD', "R_CLINCH", 
         "B_CLINCH", "R_GROUND", "B_GROUND", "R_LEG", "B_LEG") # supplementary variable, which is not included in the analysis
inc <- c("R_KD", "B_KD", "R_SIG_STR.", "B_SIG_STR.", "R_TOTAL_STR.", "B_TOTAL_STR.", 
         "R_SUB_ATT", "B_SUB_ATT", "R_CTRL", "B_CTRL", "R_HEAD", "B_HEAD", 
         "R_BODY", "B_BODY", "R_DISTANCE", "B_DISTANCE", "win_by", "last_round",
         "Fight_type", "Winner"
         )
res.famd <- part_df[, inc] %>%
  FAMD(ncp = 6, 
       sup.var = c(-2),
       graph = TRUE)

fviz_mfa_ind(res.famd, 
             habillage = "Fight_type", # color by groups 
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
) 

get_eigenvalue(res.famd)
fviz_screeplot(res.famd)
var <- get_famd_var(res.famd) 
#Coordinates of variables
head(var$coord)
# Cos2: quality of representation on the factor map
head(var$cos2)
# Contributions to the  dimensions
head(var$contrib)

# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)


#transform data into 1 fighter per row (find higher correlation between variables)

#R data
rdf <- fight %>% 
  mutate(opp = B_fighter) %>%
  select(! starts_with("b_")) %>%
  mutate(rb = "R") %>%
  rename_all(~stringr::str_replace_all(.,"^R_",""))

bdf <- fight %>% 
  mutate(opp = R_fighter) %>%
  select(! starts_with("r_")) %>%
  mutate(rb = "B") %>%
  rename_all(~stringr::str_replace_all(.,"^B_",""))

rb_sep <- rbind(rdf, bdf) %>%
  mutate(win_lose = ifelse(fighter==Winner,'1','0')) %>%
  select(-Winner)

wrong_odds <- rb_sep %>%
  subset((rb=='R' & win_lose=='0') | (rb=='B' & win_lose=='1') ) 

num <- c("KD", "SIG_STR.", "TOTAL_STR.", "SUB_ATT", "CTRL", "HEAD", 
         "BODY", "DISTANCE", "win_by", "last_round")
rb_sep[, num] %>% 
  ggpairs()


