library("TSP")

noPts <- 100
x <- c(100*runif(noPts))
y <- c(100*runif(noPts))
pts <- data.frame(x,y)

write.csv(pts, "set.csv")