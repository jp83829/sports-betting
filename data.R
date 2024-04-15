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
write.csv(inc,"fights_dm.csv", row.names = FALSE)
