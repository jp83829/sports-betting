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


# FAMD by fight_type 
# supp variables: win_by, fight_type, winner, R_fighter, B_fighter, Referee (17,18,19,28,29,30)
famdgp <- function(indata,supp) {
  res.famdby <- indata %>% 
    group_by(Fight_type) %>% 
    do(pca = FAMD(., ncp = 4, sup.var = supp, graph = F)) %>%
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
}

# do for whole dataset
famdgp(inc, c(17,18,19,28,29,30))

# do for underdog won
wrong_odds <- inc %>%
  subset(B_fighter==Winner)

famdgp(wrong_odds, c(17,18,19,28,29,30))

