library("TSP")
library("hash")
library("sets")

dataTSP <- read_TSPLIB("/home/LC/mailo01/TSP-ML-Approach/src/R/CLUSTER_DIV/main.tsp")


pointSet <- hash()
noPts <- length(dataTSP)/2
for (i in 1:noPts){
    pointSet[[toString(i)]] <- tuple(dataTSP[i], dataTSP[i+noPts])
}

dist_pts <- function(pt1,pt2){
        return(sqrt((pt1[[1]]-pt2[[1]])^2 + (pt1[[2]]-pt2[[2]])^2))
}

radius <-1.5
current_tree<- list()
checklist <-(c(1:noPts))
while (length(checklist) != 0){

    branch <- list()

    old_branch_len<- length(branch)

    startPt = checklist[1]
    branch<-append(branch, startPt)


    while(old_branch_len != length(branch)){
        
        current_branch_len <- length(branch)

        pts_to_connect <- c(branch[old_branch_len+1:current_branch_len])

        checklist <-checklist[!checklist %in% pts_to_connect]
        old_branch_len= current_branch_len


        for (connect_pt in pts_to_connect){

            for (comparePt in checklist){
                if (!comparePt %in%branch){
                    vPt1 <-values(pointSet, keys=connect_pt)
                    vPt2 <-values(pointSet, keys=comparePt)

                    if (dist_pts(vPt1,vPt2) <= 2*radius){
                        branch<-append(branch,comparePt)
			checklist <-checklist[!checklist %in% c(comparePt)]
                        
                    }
                } 
                  
            }
        }

    }
    print("one branch added. remaining vertex to be allocated")
    print(length(checklist))
    
    current_tree <- append(current_tree, list(branch))
}

current_tree


