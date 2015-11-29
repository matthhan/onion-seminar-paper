kddcup <- read.csv("../data/kddcup.data_10_percent",header=F,sep=",")
handle <- file("../data/kddcup.names")
names_vec <- readLines(handle)
close(handle)
names_vec <- names_vec[-1]
names_vec <- names_vec[1:(length(names_vec)-1)]
names_vec <- sapply(strsplit(names_vec,":"),function(blub){blub[1]})
names(kddcup) <- names_vec
data <- kddcup[sample(1:nrow(kddcup),10000),]
data <- data[c("src_bytes","dst_bytes")]
data <- scale(data)
data <- data.frame(data)
