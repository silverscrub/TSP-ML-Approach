library("TSP")
 ## Write and read data in TSPLIB format
no <- 100
 pts  <- data.frame(x=100*runif(no), y=100*runif(no))
## create TSP, ATSP and ETSP (2D)
etsp <- ETSP(pts) 
 
write_TSPLIB(etsp, file="main.tsp")
