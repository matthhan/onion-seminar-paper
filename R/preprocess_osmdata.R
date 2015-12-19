load("..//data//south_america_osm_data.Rdata")
data <- south_america[sample(1:nrow(south_america),nrow(south_america) * 0.5),]
rm(south_america)
gc()

