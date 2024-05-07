#data to be used
fights_dm <- import("fights_dm.csv")
fights_dm$Winner <- as.factor(fights_dm$Winner)


var_ban <- c("R_TOTAL_STR.", "B_TOTAL_STR.", "R_SIG_STR.", "B_SIG_STR.", 
         "R_height_cm", "B_height_cm", "R_KD", "B_KD", "R_age", "B_age",
         'R_fighter', 'B_fighter'
)

var_ftr <- c("R_SIG_STR.", "B_SIG_STR.", "R_age", "B_age", "R_SUB_ATT", "B_SUB_ATT",
             "R_CTRL", "B_CTRL", 'R_fighter', 'B_fighter'
)

var_fly <- c("R_SIG_STR.", "B_SIG_STR.", "R_height_cm", "B_height_cm", 
             "R_reach_in_cm", "B_reach_in_cm", "R_CTRL", "B_CTRL",
            'R_fighter', 'B_fighter'
)

var_hvy <- c("R_TOTAL_STR.", "B_TOTAL_STR.", "R_SIG_STR.", "B_SIG_STR.", 
             "R_height_cm", "B_height_cm", "R_reach_in_cm", "B_reach_in_cm", 
             'R_fighter', 'B_fighter'
)

var_lhvy <- c("R_TOTAL_STR.", "B_TOTAL_STR.", "R_SIG_STR.", "B_SIG_STR.", 
              "R_age", "B_age", "R_reach_in_cm", "B_reach_in_cm", "R_CTRL", "B_CTRL",
              'R_fighter', 'B_fighter'
)

var_lt <- c("R_SIG_STR.", "B_SIG_STR.", "R_height_cm", "B_height_cm",
            "R_reach_in_cm", "B_reach_in_cm", 'R_fighter', 'B_fighter'
)

var_mdl <- c("R_TOTAL_STR.", "B_TOTAL_STR.", "R_SIG_STR.", "B_SIG_STR.", 
              "R_age", "B_age", "R_SUB_ATT", "B_SUB_ATT", "R_CTRL", "B_CTRL",
             "R_HEAD", "B_HEAD", 'R_fighter', 'B_fighter'
)

var_wel <- c("R_TOTAL_STR.", "B_TOTAL_STR.", "R_SIG_STR.", "B_SIG_STR.", 
             "R_age", "B_age", "R_reach_in_cm", "B_reach_in_cm",
             "R_HEAD", "B_HEAD", "Fight_type", 'R_fighter', 'B_fighter'
)


# train the model by weight
rf <- function(weight){
  data <- fights_dm %>%
    subset(Fight_type==weight)
  
  set.seed(222)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]

  if (weight=="Flyweight Bout") {
    fly <- randomForest(Winner~var(var_fly), data=train, proximity=TRUE) 
  }
  else if (weight=="Bantamweight Bout") {
    ban <- randomForest(Winner~var_ban, data=train, proximity=TRUE) 
  }
  else if (weight=="Featherweight Bout") {
    ftr <- randomForest(Winner~var_ftr, data=train, proximity=TRUE) 
  }
  else if (weight=="Lightweight Bout") {
    lt <- randomForest(Winner~var_lt, data=train, proximity=TRUE) 
  }
  else if (weight=="Welterweight Bout") {
    wel <- randomForest(Winner~var_wel, data=train, proximity=TRUE) 
  }
  else if (weight=="Middleweight Bout") {
    mdl <- randomForest(Winner~var_mdl, data=train, proximity=TRUE) 
  }
  else if (weight=="Light Heavyweight Bout") {
    lhvy <- randomForest(Winner~var_lhvy, data=train, proximity=TRUE) 
  }
  else if (weight=="Heavyweight Bout") {
    hvy <- randomForest(Winner~var_hvy, data=train, proximity=TRUE) 
  }
  print(rf)
  
}
rf("Flyweight Bout")
