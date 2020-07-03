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

angle <- function(x,y){
  dot.prod <- x%*%y 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
}

angle_pts <- function(pt1,pt2){
    
    if (pt1[[2]] -pt2[[2]] == 0){
        if (pt1[[1]] -pt2[[1]]>0){
            return(pi/2)
        }
        if (pt1[[1]] -pt2[[1]]<0){
            return(-pi/2)
        }
        if (pt1[[1]] -pt2[[1]]==0){
            return(0)
        }
    } else{
        x<- pt1[[1]] -pt2[[1]]
        y <-pt1[[2]] -pt2[[2]]
        angle <- atan((y)/(x))
        if (x < 0 & y>0){
            angle <- pi + angle
        }
        if (x >0 & y <0){
            angle <-2*pi +angle
        }
        if (x < 0 & y<0){
            angle <- pi + angle
        }
        return(angle)
        }
}

radius <-9

pivot_point <- values(pointSet, keys =1)

angle_based <- list()
distance_based <-list()

for (i in 1:noPts){
    compare_point <- values(pointSet, keys= i)
    element <-list(list(i, dist_pts(pivot_point,compare_point), angle_pts(compare_point,pivot_point)))
    angle_based <- append(angle_based, element)
    distance_based <- append(distance_based, element)
}


angle_based <- angle_based[order(sapply(distance_based, '[[', 3))]
distance_based <- distance_based[order(sapply(distance_based,'[[',2))]
get_item <- function(item_to_get, database){
    counter <-1
    cont <- TRUE 
    while (counter <= length(database) & cont == TRUE){
        data_pt <-database[[counter]]
        if (data_pt[1] == item_to_get){
            return(data_pt)
            cont <- FALSE
        }
        counter <- counter +1
    }
}

get_pts_range_dist <- function(lower_bound, higher_bound){
    cont = TRUE
    counter <-1
    eligible_vertices <-list()
    while (counter <= length(distance_based) & cont ==TRUE){
        distance_wrt_pivot <- distance_based[[counter]]
        
        if (distance_wrt_pivot[[2]] >= lower_bound & distance_wrt_pivot[[2]]<= higher_bound){
            eligible_vertices <- append(eligible_vertices, distance_wrt_pivot[[1]])
        }
        if (distance_wrt_pivot[[2]]> higher_bound){
            cont <- FALSE
        }
        counter <- counter +1
    }
    return(eligible_vertices)
}

obtain_angle_range <- function(lower_bound, higher_bound){
    cont = TRUE
    counter <-1
    eligible_vertices <-list()
    while (counter <= length(angle_based) & cont ==TRUE){
        angle_wrt_hor <- angle_based[[counter]]
        
        if (angle_wrt_hor[[3]] >= lower_bound & angle_wrt_hor[[3]]<= higher_bound){
            eligible_vertices <- append(eligible_vertices, angle_wrt_hor[[1]])
        }
        if (angle_wrt_hor[[3]]> higher_bound){
            cont <- FALSE
        }
        counter <- counter +1
    }
    return(eligible_vertices)
}

conform_angle <- function(angle){
    if (angle <0){
        angle <- 2*pi + angle
    }
    if (angle >2*pi){
        angle <- angle - 2*pi
    }
    
    return(angle)
}

get_pts_range_angle <- function(distance_from_pivot, radius, angle){
     eligible_vertices <-list()
    if (distance_from_pivot < 2*radius){
                for (point in 1:length(angle_based)){
            eligible_vertices <- append(eligible_vertices, angle_based[[point]][1])
            }
    }else{
        if (2*radius == distance_from_pivot){
            #r = d, gives the angle range that is 180 degrees. Gives which side?
          
            higher_bound <- conform_angle(pi/2 - (pi - angle))
            lower_bound<- conform_angle(higher_bound + pi)
           
            eligible_vertices <- append(eligible_vertices, obtain_angle_range(lower_bound, higher_bound))
            
	}else{
            #r > d
                        phi <- asin(2*radius/distance_from_pivot)
            if (angle + phi > 2*pi){
                diff <-  angle + phi -2*pi 
                eligible_vertices <- append(eligible_vertices, obtain_angle_range(0,diff))
                eligible_vertices <- append(eligible_vertices, obtain_angle_range(angle-phi, 2*pi))
                
            } else {
                if (angle - phi < 0){
                    diff <- angle - phi 
                    eligible_vertices <- append(eligible_vertices, obtain_angle_range(2*pi + diff, 2*pi))
                    eligible_vertices <- append(eligible_vertices, obtain_angle_range(0, angle+phi))
                    
                }else{
                    eligible_vertices <- append(eligible_vertices, obtain_angle_range(angle-phi, angle+phi))
                
                
                
                
                }
            }
        }
    }    
    return(eligible_vertices)

}


start_time <- Sys.time()
current_tree <- list()
checklist <-(c(1:noPts))
while (length(checklist) != 0){

    branch <- list()

    old_branch_len<- length(branch)

    startPt = checklist[1]
    branch<-append(branch, startPt)

    counter =1

    while(old_branch_len != length(branch)){

        current_branch_len <- length(branch)

        start <-old_branch_len+1
        pts_to_connect <- c(branch[start:current_branch_len])


        checklist <-checklist[!checklist %in% pts_to_connect]

        old_branch_len= current_branch_len

        for (connect_pt in pts_to_connect){
            ## to yield an estimates of near by points
            pivot_data <- get_item(connect_pt, distance_based)
            dist_wrt_pivot <- pivot_data[[2]][1]
            angle_wrt_hor <-pivot_data[[3]][1]
            

            if (dist_wrt_pivot == 0){
                possible_connect <- get_pts_range_dist(0,2*radius)
                
            } else {

                possible_angle <- get_pts_range_angle(dist_wrt_pivot, radius, angle_wrt_hor)
               
                possible_dist <- get_pts_range_dist(dist_wrt_pivot-2*radius, dist_wrt_pivot+2*radius)
           	possible_connect <- intersect(possible_angle, possible_dist)
            }
            
            for (comparePt in possible_connect){

                if (!comparePt %in%branch){
                    vPt1 <-values(pointSet, keys=connect_pt)
                    vPt2 <-values(pointSet, keys=comparePt)

                    if (dist_pts(vPt1,vPt2) <= 2*radius){

                        branch<-append(branch,comparePt)
                        checklist <-checklist[!checklist %in% c(comparePt)]


                    } else {}
                } else{}
            }
        }
        counter <- counter +1

    }
    current_tree <- append(current_tree, list(branch))
}
end_time <- Sys.time()
end_time - start_time

x <- toString(length(current_tree))
cap <- nchar(x, type = "chars")
for (branchNo in 1:length(current_tree)){
    branchNum <- toString(branchNo)
    if (cap >1){
        times <- cap - nchar(branchNum, type="chars")
        if (times != 0) {
    		for (t in 1:times){
            		branchNum <- paste0("0", toString(branchNum))
        	}
	}
    }	

    pts  <- data.frame(x=c(), y=c())
    if ( length(current_tree[[branchNo]]) <=2){
        ##write solution to be the same as the output to .sol file
        file_name <- sprintf("/home/LC/mailo01/TSP-ML-Approach/src/R/CLUSTER_DIV/branch_%s_case_%s.sol",branchNum,1)
        
    	if (length(current_tree[[branchNo]]) == 1){
            
	    write(c(1,0), file_name)
	    for (pt in current_tree[[branchNo]]){
            pt_data <- values(pointSet, keys=pt)
            pts <- rbind(pts, c(pt_data[[1]],pt_data[2]))

            }
            etsp <- ETSP(pts)
            file_name <- sprintf("branch_%s_case_%s.tsp",branchNum,1)
            write_TSPLIB(etsp, file= file_name)
        }

        if (length(current_tree[[branchNo]]) == 2){
            write(c(2, 1, 0), file_name )
	    for (pt in current_tree[[branchNo]]){
            pt_data <- values(pointSet, keys=pt)
            pts <- rbind(pts, c(pt_data[[1]],pt_data[2]))

            }
            etsp <- ETSP(pts)
            file_name <- sprintf("branch_%s_case_%s.tsp",branchNum,1)
            write_TSPLIB(etsp, file= file_name)
	    
        }

    }else{

        for (pt in current_tree[[branchNo]]){
            pt_data <- values(pointSet, keys=pt) 
            pts <- rbind(pts, c(pt_data[[1]],pt_data[2]))

        }
        etsp <- ETSP(pts)
        file_name <- sprintf("branch_%s_case_%s.tsp",branchNum,1)
        write_TSPLIB(etsp, file= file_name)
    }
}

system("./solving_subcases.sh")

all_solutions <- scan("/home/LC/mailo01/TSP-ML-Approach/src/R/CLUSTER_DIV/solution_files.txt", what=character())


convert_sol_format <- function(content){
    content <- content[-1]
    for (index in 1:length(content)){
        content[[index]] <- content[[index]] + 1
    }
    return(as.TOUR(as.integer(content)))
}

get_solutions <- function(){
    len <-length(all_solutions)
    all_solution_tours <- list(list())
    for (index in 1:len){
        individual_tour <- convert_sol_format(scan(all_solutions[[index]]))
        all_solution_tours[[index]] <- individual_tour
    }
    return(all_solution_tours)
}

to_global <- function(tour_list){
	for (tour_index in 1:length(tour_list)){
		for (vertex_index in 1:length(tour_list[[tour_index]])){
			vertex <- as.integer(tour_list[[tour_index]][vertex_index])
			tour_list[[tour_index]][vertex_index] <- current_tree[[tour_index]][vertex]
		}
	}
	return(tour_list)
} 

end_pt <-function(edgeNo){
	if (edgeNo == length(branch1)){
                            end <- 1
                }else{
                        end <- edgeNo+1}
	return(end)
}

connect <- function(branch1, branch2){
 	current_best_choice <- list()
	for (edgeNo1 in 1:length(branch1)){
		end1 <- end_pt(edgeNo)
		pt1a <- branch1[[edgeNo1]]
		pt1b <- branch1[[end1]]
		edge1 <- c(pt1a,pt1b)
		pt1a_coor <- values(pointSet, keys= pt1a)  
		pt1b_coor <-values(pointSet, keys= pt1b)

		for (edgeNo2 in 1:length(branch2)){
			end2 <- end_pt(edgeNo)
                	pt2a <- branch2[[edgeNo2]]
                	pt2b <- branch2[[end2]]
                	edge2 <- c(pt2a,pt2b)
                	pt2a_cor <- values(pointSet, keys= pt2a)
                	pt2b_cor <-values(pointSet, keys= pt2b)
                	

			dist1a2a <- dist_pts(pt1a_coor,pt2a_coor)
			dist1a2b <- dist_pts(pt1a_coor,pt2b_coor)
			dist1b2a <- dist_pts(pt1b_coor,pt2b_coor)
			dist1b2b <- dist_pts(pt1b_coor,pt2b_coor)
			
			choice1_cost <-dist1a2a + dist1b2b
			choice2_cost <-dist1a2b + dist1b2a
			if ((choice1_cost) < (choice2_cost)){
				to_compare <- c(choice1_cost, pt1a2a, pt1b2b,)
			} else {
				to_compare <- c(choice2_cost, pt1a2b, pt1b2a)
			
			}

			if length(current_best_choice <- 
		}
	}
}
sol <- get_solutions()
sorted_tree <- to_global(sol)
