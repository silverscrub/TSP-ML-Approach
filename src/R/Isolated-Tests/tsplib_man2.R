library("TSP")
 ## Write and read data in TSPLIB format
 pts  <- data.frame(x=100*runif(5), y=100*runif(5))
## create TSP, ATSP and ETSP (2D)
etsp <- ETSP(pts) 
 
 write_TSPLIB(etsp, file="test_5pts.tsp")


