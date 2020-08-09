#function library for Divide and Conquer Neighborhood for TSP

#calculate the distance between point number pt1 and pt2 as listed
dist_pts <- function(pt1,pt2){
        return(sqrt((pt1[[1]]-pt2[[1]])^2 + (pt1[[2]]-pt2[[2]])^2))
}


#get the coordinate of item to get in the database
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

#get the range of points that is within given bounds in database sorted by horizontal coordinate
get_pts_range_hor<- function(lower_bound, higher_bound, database){
    cont = TRUE
    counter <-1
    eligible_vertices <-list()
    while (counter <= length(database) & cont ==TRUE){
        value <- database[[counter]]
        
        if (value[[2]] >= lower_bound & value[[2]]<= higher_bound){
            eligible_vertices <- append(eligible_vertices, value[[1]])
        }
        if (value[[2]]> higher_bound){
            cont <- FALSE
        }
        counter <- counter +1
    }
    return(eligible_vertices)
}

#get the range of points that is within given bounds in database sorted by vertical coordinate
get_pts_range_vert<- function(lower_bound, higher_bound, database){
    cont = TRUE
    counter <-1
    eligible_vertices <-list()
    while (counter <= length(database) & cont ==TRUE){
        value <- database[[counter]]
        
        if (value[[3]] >= lower_bound & value[[3]]<= higher_bound){
            eligible_vertices <- append(eligible_vertices, value[[1]])
        }
        if (value[[3]]> higher_bound){
            cont <- FALSE
        }
        counter <- counter +1
    }
    return(eligible_vertices)
}

#get list of points that are in the "box", ie. in both bounds of x and y coordinates
get_possible_pts <- function(x_current,y_current,radius, horizontal_based, vertical_based, done){
    horizontal_eligible <- get_pts_range_hor(x_current - 2*radius, x_current + 2*radius, horizontal_based)
    vertical_eligible <- get_pts_range_vert(y_current - 2*radius, y_current + 2*radius, vertical_based)

    
    eligible <- intersect(horizontal_eligible, vertical_eligible)
    eligible<- eligible[!eligible %in% done]
    
    return(eligible)
}

#this is the mechanism of choosing another edge to connect between two neighborhoods should one point in the shortest distance pair of points is occupied already.
choose_new_edge <- function(forward_edge, backward_edge, main){

    f_vertex <- forward_edge[[2]]
    b_vertex <- backward_edge[[1]]
    
    common <- forward_edge[[1]]
    compare <- current_tree[[main]]
    
    f_min <- Inf
    f_replace <- NULL
    b_min <- Inf
    b_replace <- NULL
    
    vPt_f <-values(pointSet, keys=f_vertex) 
    vPt_b <-values(pointSet, keys=b_vertex) 
    
    
    for (vertex in compare){
        vPt_c <-values(pointSet, keys =vertex)
        if (vertex != common){
            
            if (f_min == Inf){
                f_min <- dist_pts(vPt_f , vPt_c)
                b_min <- dist_pts(vPt_b, vPt_c)
                f_replace<-vertex
                b_replace<-vertex
                
                
            } else {
                dist_f <-dist_pts(vPt_f , vPt_c) 
                if (dist_f < f_min){
                    f_min <- dist_f
                    f_replace <- vertex
                } 
                
                dist_b <- dist_pts(vPt_b , vPt_c)
                if (dist_b < b_min){
                    b_min <- dist_b
                    b_replace <- vertex
                }
            }  
        }
    }
    
    if (f_min <= b_min){
        return(list("f", f_replace))
    } else {
        return(list("b", b_replace))
    }
    
}



#determining the endpoints for each neighborhood in order to set up the Concorde Solver to look for 2-end tour
end_points_det <- function(cluster_tour, connections){
    merge <- hash()
    for (pt_index in 1:length(cluster_tour)){
        main <-cluster_tour[[pt_index]]
        
        if (pt_index == length(cluster_tour)){
            forward <- cluster_tour[[1]]
            backward <- cluster_tour[[pt_index - 1]]
            
        } else {
            if (pt_index == 1){
                forward <- cluster_tour[[pt_index+1]]
                backward <- cluster_tour[[length(cluster_tour)]]

                
            } else {
                forward <- cluster_tour[[pt_index+1]]
                backward <-cluster_tour[[pt_index-1]]

                }
        }
        forward_edge <- connections[[sprintf("%s-%s",main, forward)]]
        backward_edge <- connections[[sprintf("%s-%s",backward, main)]]
        
        if (forward_edge[[1]] == backward_edge[[2]]){
            if (length(current_tree[[main]]) != 1){
                compare_result <-  choose_new_edge(forward_edge, backward_edge, main)
                if (compare_result[[1]] == "f"){
                    forward_edge[[1]] <- compare_result[[2]]
                    
                } else {
                    backward_edge[[2]] <- compare_result[[2]]
                } 
            }
        } 
        merge[[toString(main)]] <-list(backward_edge[[2]],forward_edge[[1]] )
     
    }
    return(merge)
}

#add endpoints to distance matrix 
add_end_points <- function(tsp, end_points){
    constant <- rep(c(Inf), nrow(as.matrix(tsp)) )
    constant[[end_points[[1]]]] <- 0
    constant[[end_points[[2]]]] <- 0
    tsp <-insert_dummy(tsp, 1, const =constant, inf = Inf, label = "dummy")
    return(tsp)
}

#converting local tour within neighborhood into subtour that corresponds to each point's initial coordinates
to_global <- function(tour_list, main){
        
        for (tour_index in 1:length(tour_list)){
                for (vertex_index in 1:length(tour_list[[tour_index]])){
                        vertex <- as.integer(tour_list[[tour_index]][vertex_index])
                       
                        tour_list[[tour_index]][vertex_index] <- main[[tour_index]][vertex]
                }
        }
        return(tour_list)
}

#filter the local tour so that the end points are on the end of the list that denotes tour result. This helps the obtaining the final tour by simply appending all the subtour together. Directions are taken into consideration
filter_before_merge <- function(tour_list, end_points_list){
    for (index in 1:length(tour_list)){
        sol <- tour_list[[index]]
        sol <- sol [lengths(sol) != 0]
        old <- length(sol)
        start <- end_points_list[[toString(index)]][[1]]
        end <- end_points_list[[toString(index)]][[2]]
        start_pos <- match(start, sol)
        end_pos <- match(end, sol)

        if (start_pos < end_pos){
            if (start_pos != 1 | end_pos != length(sol)){
                sol <- append(rev(sol[1:start_pos]), rev(sol[end_pos:length(sol)]))
            } 
            
        } else {
            if (start_pos == length(sol) & end_pos == 1){
                sol <- rev(sol)
            } else {
                sol <- append(rev(sol[1:end_pos]), rev(sol[start_pos:length(sol)]))
                sol <- rev(sol)
             }
        }

        if(old != length(sol)){
            print("length mismatch, error occured in formatting")
            print("number of missing elements")
            print(old - length(sol))
        }
        
        tour_list[[index]] <- sol
    }
    return(tour_list)
} 

#merge all subtours to form the final tour
merge_all <- function(formatted_all_subtours, cluster_tour){
    merged_tour <- list()
    for (branch in cluster_tour){
        merged_tour <- append(merged_tour, all_subtours[[branch]])
    }
    print(as.integer(merged_tour))
    return(as.integer(merged_tour))
}

identify_neighborhood <- function(checklist, pointSet, horizontal_based, vertical_based){
    current_tree <- list()
    done <- list()

    while (length(checklist) != 0){

    branch <- list()

    old_branch_len<- length(branch)

    startPt = checklist[1]
    branch<-append(branch, startPt)
    done <- append(done, startPt)

    counter =1

        while(old_branch_len != length(branch)){

            current_branch_len <- length(branch)

            start <-old_branch_len+1
            pts_to_connect <- c(branch[start:current_branch_len])

            checklist <-checklist[!checklist %in% pts_to_connect]

            old_branch_len= current_branch_len

            for (connect_pt in pts_to_connect){
                vPt1 <-values(pointSet, keys=connect_pt) 
                possible_connect <- get_possible_pts(vPt1[[1]],vPt1[[2]], radius, horizontal_based,vertical_based, done)

                for (comparePt in possible_connect){

                    if (!comparePt %in%branch){

                        vPt2 <-values(pointSet, keys=comparePt) 

                        if (dist_pts(vPt1,vPt2) <= 2*radius){

                            branch<-append(branch,comparePt)
                            done <- append(done,comparePt)
                            checklist <-checklist[!checklist %in% c(comparePt)]


                        } 
                    } 
                }
            }
            counter <- counter +1

        }
    current_tree <- append(current_tree, list(branch))
    }
    return(current_tree)
}