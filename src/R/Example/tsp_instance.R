data("iris") 
d <- dist(iris[-5]) 
## create a TSP 
tsp <- TSP(d) 
tsp 
## use some methods 
n_of_cities(tsp) 
labels(tsp)
image(tsp)