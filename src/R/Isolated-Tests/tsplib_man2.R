
 ## Write and read data in TSPLIB format
 pts  <- data.frame(x=100*runif(30), y=100*runif(30))
## create TSP, ATSP and ETSP (2D)
etsp <- ETSP(pts) 

plot(etsp) 
 
 write_TSPLIB(etsp, file="test.tsp")

 r <- read_TSPLIB("test.tsp") 
 plot(r)