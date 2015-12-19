#'@export
construct_ospace <- function(data,k,eps) {
  print(system.time(nearest_neighbors <- our_fast_nn(data,k,eps=eps)))
  #const_outlier_ids <- which(apply(nearest_neighbors$nn.idx,1,function(row){0 %in% row}))
  const_outlier_ids<- which(nearest_neighbors$nn.dists[,k[1]] >= eps[2])
  const_inlier_ids <- which(nearest_neighbors$nn.dists[,k[2]] <= eps[1])
  assignment <- rep("outlier_candidate",nrow(data))
  assignment[const_outlier_ids] <- "const_outlier"
  assignment[const_inlier_ids] <- "const_inlier"
  oc_ids <- which(assignment=="outlier_candidate")
  space_delimiter <- nearest_neighbors$nn.dists[oc_ids,]
  space_delimiter <- space_delimiter[,k[1]:(ncol(space_delimiter))]
  result <- structure(class="O-Space",list(assignment=assignment,
                                           oc_ids=oc_ids,
                                           space_delimiter=space_delimiter,
                                           k=k,
                                           eps=eps
  ))
}
#'@export
detect_outliers_ospace <- function(ospace,k,eps) {
  res <- ospace$assignment
  oc_assignment <- ospace$space_delimiter[,k-(ospace$k[1] - 1)] < eps
  oc_assignment <- sapply(oc_assignment,function(bool){if(bool){return("inlier")}else{return("outlier")}})
  res[ospace$oc_ids] <- oc_assignment
  return(res)
}
#'@export
comparative_outlier_analytics_ospace <- function(ospace,set_of_ocs) {
  #First iterate over all values for k and set_of_ocs. 
  #Find eps so that all elements in set_of_ocs are outliers. 
  #Then check which ocs are outliers w.r.t. that eps and k.
  #After doing this for all values of k, find the intersection.
  relative_outliers_for_k <- matrix(F,nrow=length(ospace$oc_ids),ncol=(ospace$k[2]-ospace$k[1]+1))
  for(i in 1:ncol(ospace$space_delimiter)) {
    epsvals <- ospace$space_delimiter[which(ospace$oc_ids %in% set_of_ocs),i]
    #mineps of these epsvals is the eps for which all except one  of set_of_ocs 
    #are categorized as outliers. Then we subtract a very minuscule number from
    #mineps so that every one of them is now categorized as an outlier.
    maxeps <- min(epsvals) - 0.00001
    relative_outliers_for_k[,i] <- ospace$space_delimiter[,i] > mineps
  }
  relative_outliers_forall_k <- Reduce(f = intersect,apply(relative_outliers_for_k,2,which))
  return(ospace$oc_ids[relative_outliers_forall_k])
  
}
#'@export
outlier_centric_parameter_space_exploration_ospace <- function(ospace,input_set) {
  parameter_settings <- list()
  for(k in 1:ncol(ospace$space_delimiter)) {
    input_set_correct_indexes <- which(sapply(ospace$oc_ids,function(oc_id){oc_id%in% input_set}))
    epsilon <- max(ospace$space_delimiter[input_set_correct_indexes,k])
    parameter_settings[[k]] <- list(k=k,eps=epsilon)
  }
  #Last line is only so that the result is output as a matrix  
  return(sapply(parameter_settings,as.list))
}