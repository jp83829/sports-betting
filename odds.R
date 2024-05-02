# turn odds into implied probability
am2ip <- function(podds, nodds){
  ip_p <- (100 / (abs(podds) + 100)) * 100
  ip_n <- (abs(nodds) / (abs(nodds) + 100)) * 100
  
  house_edge <- (ip_p + ip_n) - 100
  print("House Edge = ") 
  print(house_edge)
  if (house_edge <0){
    print("100% proffitable wager ratio (negative/positive):")
    print((abs(podds)/100 + 1) / (100/abs(nodds) + 1))
  }
}
