#'@export
construct_pspace <- function(ospace) {
  pnode <- function(oc_id,eps) {
    return(c(oc_id,eps))#We would return a nice fancy object here, but doing so would make constructing pspace much slower
  }
  construct_kth_list <- function(k) {
    kth_list <- ospace$space_delimiter[,k-(ospace$k[1]-1)]
    kth_list <- lapply(1:length(kth_list),function(index) {pnode(oc_id=ospace$oc_ids[index],eps=kth_list[index])})
    #Make a vector containing only the epsilon values for each pnode
    eps_values  <- sapply(kth_list,function(pnode){pnode[2]})
    #Now we can sort the kth_list by eps value
    kth_list <- kth_list[order(eps_values)]
    return(kth_list)
  }
  p_space <- lapply(ospace$k[1]:ospace$k[2],construct_kth_list)  
  p_space <- structure(class="P-Space",list(p_space=p_space,
                                            k=ospace$k,
                                            eps=ospace$eps,
                                            assignment=ospace$assignment,
                                            oc_ids=ospace$oc_ids))
}
#'@export
detect_outliers_pspace <- function(pspace,k,eps) {
  k_th_list <- pspace$p_space[[k-(pspace$k[1]-1)]]
  k_th_list_epsvals <- sapply(k_th_list,function(pnode){pnode[2]})
  binary_search_first_smaller <- function(x,vec) { 
    binary_search_first_smaller_helper <- function(x,vec,interval) {
      interval_length <- abs(interval[2]-interval[1])
      if(interval_length==1) {
        if(vec[interval[2]] < x) return(interval[2])
        else return(interval[1])
      }
      else{
        next_to_check <- floor(interval[1] + interval_length/2)
        if(vec[next_to_check] < x) {
          return(binary_search_first_smaller_helper(x,vec,c(next_to_check,interval[2])))
        } else return(binary_search_first_smaller_helper(x,vec,c(interval[1],next_to_check)))
      }
    }
    binary_search_first_smaller_helper(x,vec,c(1,length(vec)))
  }
  first_smaller <- binary_search_first_smaller(eps,k_th_list_epsvals)
  inlier_pnodes <- k_th_list[1:first_smaller]
  inlier_ids <- sapply(inlier_pnodes,function(pnode){pnode[1]})
  res <- pspace$assignment
  res[inlier_ids] <- "inlier"
  res[res=="outlier_candidate"]  <- "outlier"
  return(res)
}
#'@export
comparative_outlier_analytics_pspace <- function(pspace,set_of_ocs) {
  comparative_outliers_by_k <- lapply(pspace$p_space,function(k_th_list) {
    #First find out where the set_of_ocs are in k_th_list
    only_oc_ids <- sapply(k_th_list,function(pnode){pnode[1]})
    indices <- which(sapply(only_oc_ids,function(oc_id){oc_id %in% set_of_ocs}))
    #now find out the last of these
    last_index_in_set_of_ocs <- max(indices)
    #find out which oc_ids are listed behind the last of these ocs in k_th_list
    relevant_pnodes <- k_th_list[last_index_in_set_of_ocs:length(k_th_list)]
    only_the_oc_ids <- sapply(relevant_pnodes,function(pnode){pnode[1]})
    return(only_the_oc_ids)
  })
  return(Reduce(intersect,comparative_outliers_by_k))
  
}
#'@export
outlier_centric_parameter_space_exploration_pspace <- function(pspace,input_set,ospace,delta) {
  real_indexes <- which(sapply(pspace$oc_ids,function(oc_id){oc_id %in% input_set}))
  minimum_ocs <- sapply(1:ncol(ospace$space_delimiter),function(k) {
    index_of_min <- ospace$oc_ids[real_indexes[which.max(ospace$space_delimiter[real_indexes,k])]]
  })
  minimum_ocs_as_indexes_into_pspace <- sapply(1:length(minimum_ocs),function(i){
    which(sapply(pspace$p_space[[i]],"[",1) == minimum_ocs[i])
  })  
  delta_applied <- floor(minimum_ocs_as_indexes_into_pspace*delta)
  epsvalues <- sapply(1:length(delta_applied),function(i){pspace$p_space[[i]][[delta_applied[[i]]]][2]})
  result <- sapply(1:length(epsvalues),function(i){list(eps=epsvalues[i],k=(ospace$k[1]:ospace$k[2])[i])})
  return(result)
}