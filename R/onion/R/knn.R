#@useDynLib onion
#'@export
#'@import RANN
our_fast_nn <- function(data,k,eps) {
  return(RANN::nn2(#Dataset to do knn on
             data=data,
             #Points for which knn query is performed
             query=data,
             #Number of nearest neighbor. In our case, this is the maximum value
             #of k
             k=k[2],
             #Use kd-trees
             treetype="kd",
             #Do a "radius" search, i.e. stop searching when no neighbors have
             #been found within radius
             searchtype="standard",
             #In our case, the max value for eps should be used for radius. This
             #allows us to process const outliers quickly
             radius=eps[2],
             #This eps parameter refers not to the eps for outlier detection but
             #to the precision of the search. In our case, we want precise knn,
             #so this is set to 0.0
             eps=0.0
             #This parameter was added by us to allow for aborting the knn
             #search, once k neighbors within eps[1] have been found
             #rmin=eps[1]
             ))
}
# #Internal code that was (for the most part) copied from the RANN package. we
# #added an rmin parameter to this and the C-file, so that when at least k
# #neighbors within a distance of rmin are found, the search terminates 
# #immediately
# nn2 <- function(data, query=data, k=min(10,nrow(data)),treetype=c("kd","bd"),
#                 searchtype=c("standard","priority","radius"),radius=0.0,eps=0.0,rmin)
# {
#   dimension  <- ncol(data)
#   if(is.null(dimension)) dimension=1L
#   query_dimension  <- ncol(query)
#   if(is.null(query_dimension)) query_dimension=1L
#   
#   ND		    <- nrow(data)
#   if(is.null(ND)) ND=length(data)
#   NQ		    <- nrow(query)
#   if(is.null(NQ)) NQ=length(data)
#   
#   # Check that both datasets have same dimensionality
#   if(query_dimension != query_dimension)
#     stop("Query and data tables must have same dimensions")	
#   
#   if(k>ND)
#     stop("Cannot find more nearest neighbours than there are points")
#   
#   searchtypeInt=pmatch(searchtype[1],c("standard","priority","radius"))
#   if(is.na(searchtypeInt)) stop(paste("Unknown search type",searchtype))
#   treetype=match.arg(treetype,c("kd","bd"))
#   
#   # Coerce to matrix form
#   if(is.data.frame(data))
#     data <- unlist(data,use.names=FALSE)
#   
#   if(!length(data)) stop("no points in data!")
#   
#   # Coerce to matrix form
#   if(!is.matrix(query))
#     query <- unlist(query,use.names=FALSE)
#   
#   if(!length(query)) stop("no points in query!")
#   
#   # void get_NN_2Set(double *data, double *query, int *D, int *ND, int *NQ, int *K, double *EPS,
#   # int *nn_index, double *distances)
#   
#   results <- .C("get_NN_2Set",
#                 as.double(data),
#                 as.double(query),
#                 as.integer(dimension),
#                 as.integer(ND),
#                 as.integer(NQ),
#                 as.integer(k),
#                 as.double(eps),
#                 as.integer(searchtypeInt), 
#                 as.integer(treetype=="bd"), 
#                 as.double(radius*radius),
#                 nn.idx   = integer(k*NQ),
#                 nn       = double(k*NQ), 
#                 #Also pass our own parameter
#                 rmin     = double(rmin))
#   
#   # now put the returned vectors into (nq x k) arrays
#   nn.indexes=matrix(results$nn.idx,ncol=k,byrow=TRUE)
#   nn.dist=matrix(results$nn,ncol=k,byrow=TRUE)
#   
#   return(list(nn.idx=nn.indexes, nn.dists=nn.dist))
# }
# # 
# # 
# # 
# # 
# # #-------------old---------------- remove?
# # library("fields")
# # special_knn <- function(dataset,kmin=1,kmax=10,epsmin) {
# #   knn <- vector(mode="list",length=nrow(dataset))
# #   for(i in 1:nrow(dataset)) {
# #     print(i)
# #     knn[[i]] <- rep(Inf,kmax)
# #     number_of_small <- 0
# #     for(j in 1:nrow(dataset)) {
# #       if(i!=j) {
# #         d <- as.vector(rdist(dataset[i,],dataset[j,]))
# #         if(d < epsmin) {
# #           number_of_small <- number_of_small + 1
# #           if(number_of_small >= kmax) {
# #             knn[[i]]  <- "const_inlier"
# #             break
# #           }
# #         }
# #         bt <- knn[[i]] > d
# #         if(any(bt)) {
# #           knn[[i]][which(bt)[1]] <- d
# #         }
# #       }
# #     }
# #   }
# #   return(knn)
# # }
# # 
# # 
# # library("MASS")
# # test_data <- Aids2[3:4]
# # print(system.time(res <- special_knn(test_data,epsmin=500)))
# 
# 
# 
