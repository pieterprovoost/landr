require(rgdal)
require(rgeos)
require(geosphere)

shapes <- readOGR("temp/shapes.shp", layer="shapes")
save(shapes, file="../data/shapes.rda")