x <- data.frame(x = runif(1000), y = runif(1000))
 ## create a TSP 
 etsp <- ETSP(x) 
 etsp 
 ## use some methods 
 n_of_cities(etsp) 
 labels(etsp) 
 ## plot ETSP and solution 
 tour <- solve_TSP(etsp) 
 tour 
 plot(etsp, tour, tour_col = "red")