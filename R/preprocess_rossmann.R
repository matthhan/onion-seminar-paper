rossmann <- read.csv("..//data/rossmann.csv")
store <- read.csv("../data/store.csv")

rossmann <- rossmann[c("Customers","Sales","Store")]
store <- store[c("Store","CompetitionDistance")]
full_data <- merge(rossmann,store)

data <- full_data[sample(1:nrow(full_data),100000),]
data <- data[complete.cases(data),]
data <- scale(data)
data <- data.frame(data)
