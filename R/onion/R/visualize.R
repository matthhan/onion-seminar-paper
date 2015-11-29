#'@export
plotospace <- function(data,ospace) {
  library(ggplot2)
  newdata <- data.frame(data,ospace$assignment)
  ggplot(newdata,aes(x=Sales,y=CompetitionDistance,color=ospace.assignment)) + 
    geom_point(size=1.3) 
}
#'@export
plotassnvec <- function(data,assnvec) {
  library(ggplot2)
  newdata <- data.frame(data,assnvec)
  ggplot(newdata,aes(x=Sales,y=CompetitionDistance,color=assnvec)) + 
    geom_point(size=1.3) 
}