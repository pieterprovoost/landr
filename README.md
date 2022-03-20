# landr

R package for adding land polygons to ggplot2 maps.

## Usage

```r
library(ggplot2)
library(sf)
library(landr)

polygon <- st_as_sf(data.frame(geom = st_as_sfc("POLYGON((2.711133929147107 51.469618439821524,2.645215960397107 51.34969630390317,2.672681780709607 51.27071900624267,2.941846819772107 51.339402625554364,3.150587054147107 51.55166904313278,3.018751116647107 51.64379939797505,2.711133929147107 51.469618439821524))"), wkt = "geom"), crs = 4326)
points <- st_as_sf(data.frame(geom = st_as_sfc("MULTIPOINT (3.02124 51.65211, 3.22998 51.71342, 3.41675 51.74744, 3.35083 51.61119, 3.64746 52.23453, 3.70239 52.04573, 3.75732 51.83578, 3.68042 51.79503, 3.17230 51.26449)"), wkt = "geom"), crs = 4326)

ggplot() +
  geom_sf(data = polygon) +
  geom_sf(data = points) +
  coord_sf(crs = "ESRI:54030") +
  geom_landr(fill = "#ffffff", alpha = 0.5)
```

