library("TSP")
 ## Write test data in TSPLIB format

no <- c(20)
for(i in no){
        each <-1
        for (j in  1:each){

                pts  <- data.frame(x=100*runif(i), y=100*runif(i))
                ## create TSP, ATSP and ETSP (2D)
                etsp <- ETSP(pts)
                file_name <- "main.tsp"
                write_TSPLIB(etsp, file=file_name)

        }
}
