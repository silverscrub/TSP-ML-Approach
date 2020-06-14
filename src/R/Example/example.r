library(TSP)

concordePath = "/Users/lewismai/Dropbox/Luther/Summer-Research/Formal/TSP-Approach/src/concorde"
concorde_path(concordePath)
concorde_help()


dataset_path = "USA312"
name = "graph1.txt"
dataset = read.table(paste(dataset_path,name,sep = ""))

arr=dataset
nodelist = unique(as.vector(as.matrix(arr)))
arr_mat = matrix(0,length(nodelist),length(nodelist))
for (i in 1:length(arr[,1])){
  arr_mat[arr[i,1],arr[i,2]] = 1
  arr_mat[arr[i,2],arr[i,1]] = 1
}
arr_mat_new = arr_mat
for(i in 1:length(arr_mat[,1])){
  arr_mat_new[i,which(arr_mat[i,]==0)] = 2 #replace all zero entries with 2
}

d <- as.dist(arr_mat_new)
tsp <- TSP(d)
tsp

o <- solve_TSP(tsp, method="concorde")
labels(o)