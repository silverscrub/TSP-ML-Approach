library("TSP")
x <- data.frame(x = runif(20), y = runif(20), row.names = LETTERS[1:20])
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