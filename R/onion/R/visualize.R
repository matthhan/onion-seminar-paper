#'@export
#'@import ggplot2
plotospace <- function(data,ospace) {
  newdata <- data.frame(data,ospace$assignment)
  ggplot(newdata,aes(x=Sales,y=CompetitionDistance,color=ospace.assignment)) + 
    geom_point(size=1.3) 
}
#'@export
plotassnvec <- function(data,assnvec) {
  newdata <- data.frame(data,assnvec)
  ggplot(newdata,aes(x=Sales,y=CompetitionDistance,color=assnvec)) + 
    geom_point(size=1.3) 
}