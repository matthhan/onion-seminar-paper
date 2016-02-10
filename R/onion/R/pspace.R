#'@export
construct_pspace <- function(ospace) {
  indices <- t(sapply(ospace$oc_ids,rep,length(ospace$k[1]:ospace$k[2])))
  epsvalues <- ospace$space_delimiter
  new_ordering <- apply(epsvalues,2,order)
  apply_ordering <- function(vec,ordering){vec[ordering]}
  for(i in 1:ncol(indices)) {
    indices[,i] <- apply_ordering(indices[,i],new_ordering[,i])
  }
  for(i in 1:ncol(epsvalues)) {
    epsvalues[,i] <- apply_ordering(epsvalues[,i],new_ordering[,i])
  }
  pspace <- structure(class="P-Space",list(indices=indices,
                                           epsvalues=epsvalues,
                                           k=ospace$k,
                                           assignment=ospace$assignment,
                                           oc_ids=ospace$oc_ids))
}
#'@export 
detect_outliers_pspace <- function(pspace,k,eps) {
  translated_k <- k-(pspace$k[1]-1)
  vector_to_search_in <- pspace$epsvalues[,translated_k]
  separating_index <- binary_search_closest(vector_to_search_in,eps)  
  outlier_ids<- pspace$indices[separating_index:nrow(pspace$indices),translated_k]
  res <- pspace$assignment
  res[outlier_ids] <- "outlier"
  res[res=="outlier_candidate"]  <- "inlier"
  return(res)
}
#'@export
comparative_outlier_analytics_pspace <- function(pspace,set_of_ocs) {
  comparative_outliers_by_k <- apply(pspace$indices,2,function(k_th_list) {
    only_oc_ids <- k_th_list  
    indices <- which(sapply(only_oc_ids,function(oc_id){oc_id %in% set_of_ocs}))
    #now find out the last of these
    first_index_in_set_of_ocs <- min(indices)
    #find out which oc_ids are listed behind the last of these ocs in k_th_list
    res <- k_th_list[first_index_in_set_of_ocs:length(k_th_list)]
    return(res)
  })
  indices_comparative_outliers <- Reduce(intersect,comparative_outliers_by_k)
  assnvec <- pspace$assignment
  assnvec[indices_comparative_outliers] <- "outlier"
  assnvec[assnvec == "outlier_candidate"] <- "inlier"
  return (assnvec)
}
#'@export
outlier_centric_parameter_space_exploration_pspace<- function(pspace,input_set,ospace,delta) {
  first_outliers_by_k <- apply(pspace$indices,2,function(k_th_list) {
    only_oc_ids <- k_th_list  
    indices <- which(sapply(only_oc_ids,function(oc_id){oc_id %in% input_set}))
    #now find out the last of these
    first_index_in_set_of_ocs <- min(indices)
    return(first_index_in_set_of_ocs)
  })
  res <- list()
  for(i in 1:length(pspace$k[1]:pspace$k[2])) {
    k <- (pspace$k[1]:pspace$k[2])[i]
    epsilon <- pspace$epsvalues[first_outliers_by_k[i],i]
    newparamsetting <- list(k=k,epsilon=epsilon)
    res[[i]]  <- newparamsetting
  }
  return(res)  
}