

library("TSP")

concorde_path('/Users/lewismai/Dropbox/Luther/Summer-Research/Formal/TSP-Approach/src/concorde/TSP')
data("USCA312")
tour <- solve_TSP(USCA312, method = "concorde", control = list(clo = "-V"))
plot(tour)
