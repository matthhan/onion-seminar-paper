
#'@export 
#Import something from Rcpp. We do not use this function but we have to import
#something from Rcpp, otherwise calling into cpp does not work
#'@importFrom Rcpp sourceCpp
#Import the igraph package to be able to use a fast algorithm for the CLIQUES
#problem
#'@import igraph
#'@useDynLib Onion
construct_dspace <- function(pspace) {
  domination_graph <- build_domination_graph(pspace)
  graph_object <- igraph::graph_from_adjacency_matrix(domination_graph,mode="undirected")
  cliques <- igraph::max_cliques(graph_object)  
  cliques <- lapply(cliques,as.numeric)
  domination_groups <- lapply(cliques,function(clique){pspace$oc_ids[clique]})
  domination_groups <- lapply(domination_groups,function(domgroup){
    #Fetch one set of k_th_list. It does not matter which. Take only the oc_ids
    one_set_of_pnodes <- sapply(pspace$p_space[[1]],function(pnode){pnode[2]})
    #now we want to sort domgroup in the same order as one_set_of_pnodes.
    #It does not seem like the standard R 'sort' and 'order' functions can do this
    #For now, we use a relatively inefficient sorting algorithm
    for(i in 1:length(one_set_of_pnodes)) {
      position_in_domgroup <- which(one_set_of_pnodes[i]==domgroup)
      if(length(position_in_domgroup)==0) next
      else {
        #swap the values. I know that we could do addswap, but this algorithm is not final anyway
        temp <- domgroup[i]
        domgroup[i] <- domgroup[position_in_domgroup]
        domgroup[position_in_domgroup] <- temp
      }
    }
    return(domgroup)
  })
  #Now each domination group D is sorted so that D[i] dominates D[i+1]
  return(structure(class="D-Space",list(domination_forest=domination_groups,
                                        oc_ids=pspace$oc_ids,
                                        eps=pspace$eps,
                                        k=pspace$k,
                                        assignment=pspace$assignment)))
}

#The actual domination graph construction is implemented in C++ because the 
#previous R implementation took far too long. For e.g. 100000 2d data points,
#the R implementation took 2 minutes, while the cpp implementation was finished
#in half a second
build_domination_graph <- function(pspace) {
  #First we have to format the pspace structure in such a way that the cpp program can use it
  list_oc_ids <- pspace$oc_ids
  pspace_without_eps <- t(sapply(pspace$p_space,function(row) {sapply(row,function(pnode){as.integer(pnode[1])})}))
  make_domination_graph(pspace = pspace_without_eps,list_oc_ids = as.integer(list_oc_ids))  
}

#currently only works in n*log(n). We should maybe save the eps values in dspace
#somehow, so that they are acessible by outlier_id
#'@export
detect_outliers_dspace <- function(dspace,eps,k,ospace) {
  epsvalue <- function(oc_id) {
    ospace_pos <- which(ospace$oc_ids==oc_id)  
    res <- ospace$space_delimiter[ospace_pos,k- (ospace$k[1]-1)]
    return(res)  
  }
  last_inliers <- sapply(dspace$domination_forest,function(domtree){
    #copypaste the binary search from pspace
    binary_search_first_smaller <- function(x,vec) { 
      binary_search_first_smaller_helper <- function(x,vec,interval) {
        interval_length <- abs(interval[2]-interval[1])
        if(interval_length==1) {
          if(epsvalue(vec[interval[2]]) < x) return(interval[2])
          else return(interval[1])
        }
        else{
          next_to_check <- floor(interval[1] + interval_length/2)
          if(epsvalue(vec[next_to_check]) < x) {
          return(binary_search_first_smaller_helper(x,vec,c(next_to_check,interval[2])))
          } else return(binary_search_first_smaller_helper(x,vec,c(interval[1],next_to_check)))
        }
      }
      binary_search_first_smaller_helper(x,vec,c(1,length(vec)))
    }        
    return(binary_search_first_smaller(eps,domtree))
  })
  inlier_lists <- sapply(1:length(dspace$domination_forest),function(index){
    dspace$domination_forest[[index]][1:last_inliers[index]]
    })
  #Now we have the lists of outliers for each domtree, but we only need one list with all of the outliers
  single_list_of_inliers <- unique(unlist(inlier_lists))
  assignmentvector <- ospace$assignment
  assignmentvector[single_list_of_inliers] <- "inlier"
  assignmentvector[assignmentvector=="outlier_candidate"] <- "outlier"
  return(assignmentvector)
}
#'@export
comparative_outlier_analytics_dspace <- function(dspace,input_set) {
    last <- function(vec){return(vec[length(vec)])}
    #Note this part of the algorithm is currently linear in the number of outliers
    weakest_outlier <- sapply(dspace$domination_forest,function(domtree){last(which(sapply(domtree,function(node){node%in%input_set})))})
    #replace integer(0) by 0
    weakest_outlier <- sapply(weakest_outlier,function(wo){if(length(wo)==0){return(0)}else{return(wo)}})
    #Get all outliers after the weakest outlier in the domtrees
    weakest_outlier <- weakest_outlier + 1
    outliers_by_domtree <- lapply(1:length(weakest_outlier),function(index){
      if(weakest_outlier[index]==1){return(NA)}
      domtree <- dspace$domination_forest[[index]]
      return(domtree[(weakest_outlier[index]):length(domtree)])
    })
    comparative_outliers <- unlist(outliers_by_domtree)
    comparative_outliers <- comparative_outliers[!is.na(comparative_outliers)]
    return(unique(comparative_outliers))
}
