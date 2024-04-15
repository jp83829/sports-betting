inc <- import("fights_dm.csv")

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
# supp variables: win_by, fight_type, winner, R_fighter, B_fighter (17,18,19,28,29)
res.famdby <- inc %>% 
  group_by(Fight_type) %>% 
  do(pca = FAMD(., ncp = 8, sup.var = c(17,18,19,28,29), graph = F)) %>%
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

  print("===================================================")
}

# FAMD by fight_type (whole) - underdog won
wrong_odds <- inc %>%
  subset(B_fighter==Winner)

res.famdby <- wrong_odds %>% 
  group_by(Fight_type) %>% 
  do(pca = FAMD(., ncp = 8, sup.var = c(17,18,19,28,29), graph = F)) %>%
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
  
  print("===================================================")
}


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

# FAMD by fight_type (underdog won transformed)
res.famdby <- wrong_odds %>% 
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

#final dataset based on FAMD
fin.var <- 
