
#'@export 
construct_dspace <- function(pspace) {
  print("building domination graph")
  domination_graph <- build_domination_graph(pspace)
  print("creating graph object")
  graph_object <- igraph::graph_from_adjacency_matrix(domination_graph,mode="undirected")
  print("executing cliques algorithm")
  cliques <- igraph::max_cliques(graph_object)  
  cliques <- lapply(cliques,as.numeric)
  domination_groups <- lapply(cliques,function(clique){pspace$oc_ids[clique]})
  domination_groups <- lapply(domination_groups,function(domgroup){
    #Fetch one set of k_th_list. It does not matter which. Take only the oc_ids
    one_set_of_pnodes <- sapply(pspace$p_space[[1]],function(pnode){pnode[2]})
    #now we want to sort domgroup in the same order as one_set_of_pnodes.
    #It does not seem like the standard R 'sort' and 'order' functions can do this
    #For now, we use a relatively inefficient sorting algorithm
    sorted_until_here <- 1
    for(i in 1:length(one_set_of_pnodes)) {
      position_in_domgroup <- which(one_set_of_pnodes==domgroup[i])
      if(length(position_in_domgroup)==0) next
      else {
        #swap the values. I know that we could do addswap, but this algorithm is not final anyway
        temp <- domgroup[sorted_until_here]
        domgroup[sorted_until_here] <- domgroup[position_in_domgroup]
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

#Creates a domination Graph. Note that the domination Graphs we use are
#different from the ones in the paper: our dominations graphs have an edge
#between two ocs a, b, iff a dominates b or b dominates a. Their domination
#graphs have an edge if this is not the case.
#'@import igraph
#'@export
build_domination_graph <- function(pspace) {
  #Returns True iff the i-th outlier candidate comes before the j-th outlier
  #candidate in the k-th list
  k_dominates <- function(i,j,k,pspace) {
    k_th_list <- pspace$p_space[[k]]
    for(index in 1:length(k_th_list)) {
      pnode <- k_th_list[[index]]
      if(pnode[1] == pspace$oc_ids[i]) return(T) #pnode[1] is the oc_id of the pnode
      if(pnode[1] == pspace$oc_ids[j]) return(F)
    }
  }
  #Returns True iff i k-dominates j for all k in [kmin,kmax]
  dominates  <- function(i,j,pspace) {
    for(index in 1:length(pspace$p_space)) {
      if(!k_dominates(pspace=pspace,i=i,j=j,k=index)) return(F)
    }
    return(T)
  }
  dominates_by_transitivity <- function(i,j,domination_graph) {
     for(h in 1:nrow(domination_graph)) {
       if(domination_graph[i,h] & domination_graph[h,j])  {
         return(T)
       }
     }
     return(F)
  }
  number_ocs <- length(pspace$p_space[[1]])
  #Embed the domination graph in a rectangular matrix where there is an edge
  #between oc i and oc j iff domination_graph[i,j] is True
  domination_graph <- matrix(F,nrow=number_ocs,ncol=number_ocs)
  for(i in 1:number_ocs) {
    for(j in 1:number_ocs) {
      if(domination_graph[i,j]) next
      if(i==j) next #No cycles
      if(dominates_by_transitivity(i,j,domination_graph) || dominates(i,j,pspace)) {
        domination_graph[i,j] <- T
        domination_graph[j,i] <- T
      }
    }
  }
  return(domination_graph)
}

#currently only works in n*log(n). We should maybe save the eps values in dspace somehow, so that they are
#acessible by outlier_id
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