#data to be used
fights_dm <- import("fights_dm.csv")

var_ban <- c("R_TOTAL_STR.", "B_TOTAL_STR.", "R_SIG_STR.", "B_SIG_STR.", 
         "R_height_cm", "B_height_cm", "R_KD", "B_KD", "R_age", "B_age",
         "Fight_type", "Winner", 'R_fighter', 'B_fighter'
)

var_ftr <- c("R_SIG_STR.", "B_SIG_STR.", "R_age", "B_age", "R_SUB_ATT", "B_SUB_ATT",
             "R_CTRL", "B_CTRL", "Fight_type", "Winner", 'R_fighter', 'B_fighter'
)

var_fly <- c("R_SIG_STR.", "B_SIG_STR.", "R_height_cm", "B_height_cm", 
             "R_reach_in_cm", "B_reach_in_cm", "R_CTRL", "B_CTRL",
            "Fight_type", "Winner", 'R_fighter', 'B_fighter'
)

var_hvy <- c("R_TOTAL_STR.", "B_TOTAL_STR.", "R_SIG_STR.", "B_SIG_STR.", 
             "R_height_cm", "B_height_cm", "R_reach_in_cm", "B_reach_in_cm", 
             "Fight_type", "Winner", 'R_fighter', 'B_fighter'
)

var_lhvy <- c("R_TOTAL_STR.", "B_TOTAL_STR.", "R_SIG_STR.", "B_SIG_STR.", 
              "R_age", "B_age", "R_reach_in_cm", "B_reach_in_cm", "R_CTRL", "B_CTRL",
              "Fight_type", "Winner", 'R_fighter', 'B_fighter'
)

var_lt <- c("R_SIG_STR.", "B_SIG_STR.", "R_height_cm", "B_height_cm",
            "R_reach_in_cm", "B_reach_in_cm","Fight_type", "Winner", 'R_fighter', 'B_fighter'
)

var_mdl <- c("R_TOTAL_STR.", "B_TOTAL_STR.", "R_SIG_STR.", "B_SIG_STR.", 
              "R_age", "B_age", "R_SUB_ATT", "B_SUB_ATT", "R_CTRL", "B_CTRL",
             "R_HEAD", "B_HEAD", "Fight_type", "Winner", 'R_fighter', 'B_fighter'
)

var_wel <- c("R_TOTAL_STR.", "B_TOTAL_STR.", "R_SIG_STR.", "B_SIG_STR.", 
             "R_age", "B_age", "R_reach_in_cm", "B_reach_in_cm",
             "R_HEAD", "B_HEAD", "Fight_type", "Winner", 'R_fighter', 'B_fighter'
)

