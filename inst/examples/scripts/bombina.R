library(rgdal)

nc <- rgdal::readOGR("data/shapes/EvoZooDeb_cities.shp", "EvoZooDeb_cities", verbose = FALSE)
proj4string(nc) <- CRS("+proj=longlat +datum=WGS84")
plot(nc)

print("Worflow Finished")
