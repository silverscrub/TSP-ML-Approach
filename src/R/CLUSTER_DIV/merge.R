library("TSP")

all_solutions <- scan("/home/LC/mailo01/TSP-ML-Approach/src/R/CLUSTER_DIV/solution_files.txt", what=character())

convert_sol_format <- function(content){
    content <- content[-1]
    for (index in 1:length(content)){
        content[[index]] <- content[[index]] + 1
    }
    return(as.integer(content))
}

