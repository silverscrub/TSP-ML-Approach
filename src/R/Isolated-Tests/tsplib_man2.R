library("TSP")
 ## Write and read data in TSPLIB format
multiplier <- 100
number <- 10
 pts  <- data.frame(x=multiplier*runif(number), y=multiplier*runif(number))
## create TSP, ATSP and ETSP (2D)
etsp <- ETSP(pts) 
 
 write_TSPLIB(etsp, file="test_10pts.tsp")


