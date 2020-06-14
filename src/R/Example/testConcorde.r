install.packages("TSP")

library("TSP")

concorde_path('/Users/lewismai/Dropbox/Luther/Summer-Research/Formal/TSP-Approach/src/concorde/TSP')
data("USCA312")
solve_TSP(USCA312, method = "concorde", control = list(clo = "-V"))