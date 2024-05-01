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
  ramify,
  randomForest
  )
