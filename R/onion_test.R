setwd("~/Desktop/Seminar/R")
source("preprocess_rossmann.R")
source("preprocess_osmdata.R")
system.time(ospace <- construct_ospace(data,k=c(3,6),eps=c(0.05,0.1)))
system.time(pspace <- construct_pspace(ospace))
system.time(dspace <- construct_dspace(pspace))

#Test outlier detections---------------------------------
res1 <- detect_outliers_ospace(ospace = ospace,k = 7,eps = 0.4)
res2 <- detect_outliers_pspace(pspace = pspace,k = 7,eps = 0.4)
res3 <- detect_outliers_dspace(dspace = dspace,k = 7,eps = 0.4,ospace = ospace)
#Test comparative outliers----------------------------------
input_set <- sample(ospace$oc_ids,5)
comp1 <- comparative_outlier_analytics_ospace(ospace = osapce,set_of_ocs = input_set)
comp2 <- comparative_outlier_analytics_pspace(pspace = pspace,set_of_ocs = input_set)
comp3 <- comparative_outlier_analytics_dspace(dspace = dspace,input_set = input_set)
#Test outlier-centric parameter space exploration-----------
input_set <- sample(ospace$oc_ids,2)
pse1 <- outlier_centric_parameter_space_exploration_ospace(ospace = ospace,input_set = input_set)
pse2 <- outlier_centric_parameter_space_exploration_pspace(pspace = pspace,input_set = input_set,ospace=ospace,delta=1)
#pse3 <- outlier_centric_parameter_space_exploration_dspace(dspace = dspace,input_set = input_set)