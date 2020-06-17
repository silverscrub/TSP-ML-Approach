library("TSP")
x <- data.frame(x = runif(50), y = runif(50))
## create a TSP
etsp <- ETSP(x)
etsp
## use some methods
n_of_cities(etsp)
labels(etsp)
## plot ETSP and solution
tour <- solve_TSP(etsp)
tour

## set radius

plot(etsp, tour, tour_col = "red")