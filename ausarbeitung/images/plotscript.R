data <- read.csv("../../results/vary_dataset_size.csv",header=T,sep=",")
library("ggplot2")
#ospace construction compared to knn query
ggplot(data,aes(x=dataset_size)) + 
  geom_line(aes(y=knn_time,linetype="knn_time")) + geom_point(aes(y=knn_time))+
  geom_line(aes(y=ospace_time,linetype="ospace_time")) + geom_point(aes(y=ospace_time)) + 
  guides(linetype=guide_legend(title="Processing Time"))+ 
  scale_linetype_discrete(labels=c("K-Nearest-Neighbors Query","O-Space Construction Overhead"))+ 
  labs(x="Dataset Size (Pairs)",y="Time (Seconds)")
#space constructions over dataset size
ggplot(data,aes(x=dataset_size)) +
  geom_line(aes(y=ospace_time,linetype="ospace_time")) + geom_point(aes(y=ospace_time)) +
  geom_line(aes(y=pspace_time,linetype="pspace_time")) + geom_point(aes(y=pspace_time)) +
  geom_line(aes(y=dspace_time,linetype="dspace_time")) + geom_point(aes(y=dspace_time)) +
  guides(linetype=guide_legend(title="Processing Time"))+ 
  scale_linetype_discrete(labels=c("D-Space Construction","O-Space Construction","P-Space Construction"))+ 
  labs(x="Dataset Size (Pairs)",y="Time (Seconds)")
#dspace and pspace construction over number of outlier candidates
ggplot(data,aes(x=outlier_candidates)) +
  geom_line(aes(y=pspace_time,linetype="pspace_time")) + geom_point(aes(y=pspace_time)) +
  geom_line(aes(y=dspace_time,linetype="dspace_time")) + geom_point(aes(y=dspace_time)) +
  guides(linetype=guide_legend(title="Processing Time"))+ 
  scale_linetype_discrete(labels=c("D-Space Construction","P-Space Construction"))+ 
  labs(x="Number of Outlier Candidates",y="Time (Seconds)")
#Number of outliers dataset size
ggplot(data,aes(x=dataset_size)) +
  geom_point(aes(y=outlier_candidates))+
  labs(x="Dataset Size",y="Number of Outlier Candidates")
#read dataset on pspace and dspace durations by outlier candidates
data <- read.csv("../../results/vary_parameters_outlier_candidates.csv",header=T,sep=",")
ggplot(data,aes(x=outlier_candidates)) + 
  geom_line(aes(y=dspace_construction)) + geom_point(aes(y=dspace_construction)) + 
  scale_y_log10(breaks=c(1,10,100)) +
  labs(y="Time (Seconds)",x="Number of Outlier Candidates")
data <- read.csv("../../results/vary_k_max.csv",header=T,sep=",")
ggplot(data,aes(x=k_max)) + 
  geom_line(aes(y=duration_knn_query)) + geom_point(aes(y=duration_knn_query)) + 
  labs(y="Time (Seconds)",x="Value for kmax") + expand_limits(y=0)
















