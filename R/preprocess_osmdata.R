load("..//data//south_america_osm_data.Rdata")
data <- south_america[sample(1:nrow(south_america),50000000),]
rm(south_america)
gc()

