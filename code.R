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

# import data sets
fight_raw <- import("ufc_data_till_UFC_292.csv")
fighter_raw <- import("ufc-fighters-statistics.csv")

# age function
age_calc_miss <- function(dob, enddate = Sys.Date(), units = "months", precise = TRUE){
  retval <- rep(NA_real_, length(dob))
  miss <- is.na(dob)
  retval[!miss] <- eeptools::age_calc(dob = dob[!miss], 
                                      enddate = enddate, 
                                      units = units, 
                                      precise = precise)
  retval
  }

# pre-process demographics data set 
fighter <- fighter_raw %>%
  mutate(age = floor(age_calc_miss(date_of_birth, units = "years"))) %>%
  select(name, height_cm, reach_in_cm, stance, age) %>%
  subset(! name %in% c(name[duplicated(name)])) %>%
  mutate_at(c("height_cm","age"), ~ifelse(is.na(.), median(.,na.rm=T), .)) %>%
  mutate_at("reach_in_cm", ~ifelse(is.na(.), height_cm, .)) 


# rename for merging data
fighter_R <- fighter %>%
  rename_all( list(~paste0("R_", .)))

fighter_B <- fighter %>%
  rename_all( list(~paste0("B_", .)))

# pre-process/data merge
count <- c('R_SIG_STR.', 'B_SIG_STR.', 'R_TOTAL_STR.', 'B_TOTAL_STR.', 'R_TD', 
           'B_TD', "R_HEAD", "B_HEAD", "R_BODY", "B_BODY", "R_DISTANCE", "B_DISTANCE") #character "x of y" to be converted to percentage
time <- c('R_CTRL', 'B_CTRL') #character time span to be converted to numeric

fight <- fight_raw %>% 
  mutate_at(vars(count),
            ~ as.numeric(str_split_i(., " of ", 1))/as.numeric(str_split_i(., " of ", 2)) ) %>%
  mutate_at(vars(time),
            ~ as.numeric(str_split_i(., ":", 1))+as.numeric(str_split_i(., ":", 2))/60) %>%
  mutate_all(~ifelse(is.nan(.), 0, .)) %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%
  left_join(fighter_R,
            by = c("R_fighter" = "R_name"),
            suffix = c(".x", ".y"),
            keep = NULL) %>%
  left_join(fighter_B,
            by = c("B_fighter" = "B_name"),
            suffix = c(".x", ".y"),
            keep = NULL)   %>%
  subset(Winner!="" & ! grepl('Women',Fight_type)) %>%
  mutate_at("Fight_type", ~ifelse(grepl("Flyweight", .), "Flyweight Bout", .)) %>%
  mutate_at("Fight_type", ~ifelse(grepl("Bantamweight", .), "Bantamweight Bout", .)) %>%
  mutate_at("Fight_type", ~ifelse(grepl("Featherweight", .), "Featherweight Bout", .)) %>%
  mutate_at("Fight_type", ~ifelse(grepl("Lightweight", .), "Lightweight Bout", .)) %>%
  mutate_at("Fight_type", ~ifelse(grepl("Welterweight", .), "Welterweight Bout", .)) %>%
  mutate_at("Fight_type", ~ifelse(grepl("Middleweight", .), "Middleweight Bout", .)) %>%
  mutate_at("Fight_type", ~ifelse(grepl("Light Heavyweight", .), "Light Heavyweight Bout", .)) %>%
  mutate_at("Fight_type", ~ifelse(grepl("Heavyweight", .) & ! grepl("Light", .), "Heavyweight Bout", .)) %>% 
  subset(Winner!="" & ! is.na(R_height_cm) & ! is.na(B_height_cm) &
           Fight_type %in% c("Flyweight Bout", "Bantamweight Bout",
                            "Featherweight Bout", "Lightweight Bout", "Welterweight Bout",
                            "Middleweight Bout", "Light Heavyweight Bout", "Heavyweight Bout")) 

# final data for analysis
View(fight)

# sampling for testing
set.seed(2024)
part_df <- fight %>% 
  slice_sample(n=100)

# FAMD - general (whole)
exc <- c('R_SIG_STR_pct', 'B_SIG_STR_pct', 'R_TD_pct', 'B_TD_pct', 'R_REV', 
         'B_REV', 'last_round_time', 'R_fighter', 'B_fighter', "Format",
         "Referee", "date", "location", 'Winner', 'R_TD', 'B_TD', "R_CLINCH", 
         "B_CLINCH", "R_GROUND", "B_GROUND", "R_LEG", "B_LEG") # supplementary variable, which is not included in the analysis
inc <- c("R_KD", "B_KD", "R_SIG_STR.", "B_SIG_STR.", "R_TOTAL_STR.", "B_TOTAL_STR.", 
         "R_SUB_ATT", "B_SUB_ATT", "R_CTRL", "B_CTRL", "R_HEAD", "B_HEAD", 
         "R_BODY", "B_BODY", "R_DISTANCE", "B_DISTANCE", "win_by", "last_round",
         "Fight_type", "Winner", "R_height_cm", "R_reach_in_cm", "R_stance", "R_age",
         "B_height_cm", "B_reach_in_cm", "B_stance", "B_age"
         )

res.famd <- part_df[, inc] %>%
  FAMD(ncp = 10, 
       sup.var = c(-10, -9), #supp variables: Fight_type, Winner
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
rank <-
  var$contrib %>%
  data.frame %>%
  arrange(desc(Dim.1), desc(Dim.2), desc(Dim.3), desc(Dim.4))
head(var$contrib)

# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)


# FAMD by fight_type (whole)

par(mfrow=c(8,2), mar=c(4,4,2,1))

# Separate PCA plot for each Fight type
# Apply our defined PCA-function where each unique INDICES are handled as a separate function call
by(part_df, INDICES=part_df$Fight_type, FUN=function(z){
  res.famd <- FAMD(z[, inc], ncp = 4, sup.var = c(-10), graph = T)

  var <- get_famd_var(res.famd) 
  # Contributions to the  dimensions
  rank <-
    var$contrib %>%
    as_tibble() %>%
    setNames(c('PCA1', 'PCA2', 'PCA3', 'PCA4')) %>%
    arrange(desc(PCA1), desc(PCA2), desc(PCA3), desc(PCA4))
  
  ?data.table
  # Plot of variables
  fviz_famd_var(res.famd, repel = TRUE)
  # Contribution to the first dimension
  fviz_contrib(res.famd, "var", axes = 1)
  # Contribution to the second dimension
  fviz_contrib(res.famd, "var", axes = 2)
  })

# Color annotation
# Use numeric fields for the PCA
pca <- FAMD(part_df[, inc], ncp = 6, sup.var = c(-10), graph = F)
plot(pca$x[,1:2], pch=16, col=as.numeric(part_df[,"Fight_type"]), main="Color annotation") # 2 first principal components
legend("bottom", pch=16, col=unique(as.numeric(part_df[,"Fight_type"])), legend=unique(part_df[,"Fight_type"]))




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

# FAMD by fight_type (whole transformed)
# FAMD by fight_type (wrong odds transformed)
