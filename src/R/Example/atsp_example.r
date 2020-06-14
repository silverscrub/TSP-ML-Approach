

data <- matrix(runif(10^2), ncol = 10, dimnames = list(1:10, 1:10)) 
atsp <- ATSP(data) 
atsp
## use some methods 
n_of_cities(atsp) 
labels(atsp) 
## calculate a tour 
tour <- solve_TSP(atsp, method = "concorde", as_TSP=TRUE,control = list(clo = "-v -V")) 
tour 
tour_length(tour) 
image(atsp, tour)