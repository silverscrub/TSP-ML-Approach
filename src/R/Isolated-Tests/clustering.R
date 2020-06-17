library("TSP")
noPts <- 100
x <- c(100*runif(noPts))
y <- c(100*runif(noPts))
pts <- data.frame(x,y)

## create a TSP
etsp <- ETSP(pts)
etsp
## use some methods
n_of_cities(etsp)
labels(etsp)
## plot ETSP and solution
tour <- solve_TSP(etsp, method="concorde")
tour

## set radius
r <- 0.1


print(x)
print(y)

plot(etsp, tour, tour_col = "red")
symbols(x, y, circles=rep(4,noPts), add=T, inches=F)

