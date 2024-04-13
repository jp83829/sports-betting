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
  eeptools,
  ramify 
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

# variables 
inc <- c("R_KD", "B_KD", "R_SIG_STR.", "B_SIG_STR.", "R_TOTAL_STR.", "B_TOTAL_STR.", 
         "R_SUB_ATT", "B_SUB_ATT", "R_CTRL", "B_CTRL", "R_HEAD", "B_HEAD", 
         "R_BODY", "B_BODY", "R_DISTANCE", "B_DISTANCE", "win_by", 
         "Fight_type", "Winner", "R_height_cm", "R_reach_in_cm", "R_stance", "R_age",
         "B_height_cm", "B_reach_in_cm", "B_stance", "B_age", 'R_fighter', 'B_fighter'
         )

inc <- fight[, inc]

# FAMD - general (whole)
res.famd <- inc %>%
  FAMD(ncp = 16, 
       sup.var = 20, #supp variables: Winner
       graph = F)

fviz_mfa_ind(res.famd, 
             habillage = "Fight_type", # color by groups 
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
) 

get_eigenvalue(res.famd)
var <- get_famd_var(res.famd) 

# Contributions to the  dimensions
rank <-
  var$contrib %>%
  data.frame 

# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)


# FAMD by fight_type (whole)

# Separate PCA plot for each Fight type
# Apply our defined PCA-function where each unique INDICES are handled as a separate function call
res.famdby <- inc %>% 
  group_by(Fight_type) %>% 
  do(pca = FAMD(., ncp = 6, sup.var = c(17,18,19,28,29), graph = F)) %>%
  mutate(contrib = list(list()))

for (i in 1:8) {
  print(res.famdby[[1]][[i]])
  print("---------------------")
  
  get_eigenvalue(res.famdby[[2]][[i]]) %>% print()
  
  # Contributions to the dimensions
  var <- get_famd_var(res.famdby[[2]][[i]]) 
  res.famdby[[3]][[i]] <- var 
  apply(var$contrib,2,max) %>% print()
  rownames(var$contrib)[argmax(res.famdby[[3]][[i]][["contrib"]], rows = F)] %>% print()

  par(mfrow=c(1,3))
  # Plot of variables
  fviz_famd_var(res.famdby[[2]][[i]], repel = TRUE) %>% print()
  # Contribution to the first dimension
  fviz_contrib(res.famdby[[2]][[i]], "var", axes = 1) %>% print()
  # Contribution to the second dimension
  fviz_contrib(res.famdby[[2]][[i]], "var", axes = 2) %>% print()
  print("===================================================")
}

# Color annotation
# Use numeric fields for the PCA
pca <- FAMD(part_df[, inc], ncp = 6, sup.var = c(-10), graph = F)
plot(pca$x[,1:2], pch=16, col=as.numeric(part_df[,"Fight_type"]), main="Color annotation") # 2 first principal components
legend("bottom", pch=16, col=unique(as.numeric(part_df[,"Fight_type"])), legend=unique(part_df[,"Fight_type"]))




#transform data into 1 fighter per row (find higher correlation between variables)

#R data
rdf <- inc %>% 
  mutate(opp = B_fighter) %>%
  select(! starts_with("b_")) %>%
  mutate(rb = "R") %>%
  rename_all(~stringr::str_replace_all(.,"^R_",""))

bdf <- inc %>% 
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
res.famdby <- rb_sep %>% 
  group_by(Fight_type) %>% 
  do(pca = FAMD(., ncp = 5, sup.var = c(9,10,15,16,17), graph = F)) %>%
  mutate(contrib = list(list()))

for (i in 1:8) {
  print(res.famdby[[1]][[i]])
  print("---------------------")
  
  get_eigenvalue(res.famdby[[2]][[i]]) %>% print()
  
  # Contributions to the dimensions
  var <- get_famd_var(res.famdby[[2]][[i]]) 
  res.famdby[[3]][[i]] <- var 
  apply(var$contrib,2,max) %>% print()
  rownames(var$contrib)[argmax(res.famdby[[3]][[i]][["contrib"]], rows = F)] %>% print()
  
  par(mfrow=c(1,3))
  # Plot of variables
  fviz_famd_var(res.famdby[[2]][[i]], repel = TRUE) %>% print()
  # Contribution to the first dimension
  fviz_contrib(res.famdby[[2]][[i]], "var", axes = 1) %>% print()
  # Contribution to the second dimension
  fviz_contrib(res.famdby[[2]][[i]], "var", axes = 2) %>% print()
  print("===================================================")
}

# FAMD by fight_type (wrong odds transformed)
res.famdby <- wrong_odds %>% 
  group_by(Fight_type) %>% 
  do(pca = FAMD(., ncp = 5, sup.var = c(9,10,15,16,17,18), graph = F)) %>%
  mutate(contrib = list(list()))

for (i in 1:8) {
  print(res.famdby[[1]][[i]])
  print("---------------------")
  
  get_eigenvalue(res.famdby[[2]][[i]]) %>% print()
  
  # Contributions to the dimensions
  var <- get_famd_var(res.famdby[[2]][[i]]) 
  res.famdby[[3]][[i]] <- var 
  apply(var$contrib,2,max) %>% print()
  rownames(var$contrib)[argmax(res.famdby[[3]][[i]][["contrib"]], rows = F)] %>% print()
  
  par(mfrow=c(1,3))
  # Plot of variables
  fviz_famd_var(res.famdby[[2]][[i]], repel = TRUE) %>% print()
  # Contribution to the first dimension
  fviz_contrib(res.famdby[[2]][[i]], "var", axes = 1) %>% print()
  # Contribution to the second dimension
  fviz_contrib(res.famdby[[2]][[i]], "var", axes = 2) %>% print()
  print("===================================================")
}
