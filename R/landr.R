cache_env <- new.env(parent = emptyenv())

#' @title Add land polygons to ggplot2 maps
#'
#' @description
#' This package adds land polygons from OpenStreetMap to ggplot2 maps.
#'
#' @docType package
#' @name landr
#' @import ggplot2 sf memoise
#' @author Pieter Provoost, \email{pieterprovoost@gmail.com}
NULL

#' This geom adds land polygons from OpenStreetMap to ggplot2 maps.
#'
#' @param after index of the layer after which the land polygons should be added. Set to 0 to add at the bottom.
#' @export
geom_landr <- function(after = 0, ...) {
  structure(list(after = after, ...), class = "landr")
}

get_clip <- function(bbox, crs) {
  st_crop(st_transform(get_land_polygons(), crs = crs), bbox)
}

get_clip_cached <- memoise::memoise(get_clip)

#' @export
ggplot_add.landr <- function(object, plot, object_name) {
  buffer <- 0.1
  b <- ggplot_build(plot)
  xrange <- b$layout$panel_params[[1]]$x_range
  yrange <- b$layout$panel_params[[1]]$y_range
  dx <- xrange[2] - xrange[1]
  dy <- yrange[2] - yrange[1]
  bbox <- c(xmin = xrange[1] - dx * buffer, xmax = xrange[2] + dx * buffer, ymin = yrange[1] - dy * buffer, ymax = yrange[2] + dy * buffer)
  crs <- ifelse(is.null(b$layout$coord$crs), 4326, b$layout$coord$crs)
  clip <- get_clip_cached(bbox, crs)
  new_coord <- plot$coordinates
  new_coord$limits$x <- xrange
  new_coord$limits$y <- yrange
  object$data <- clip
  layer <- do.call(geom_sf, object[names(object) != "after"])
  plot$layers <- append(plot$layers, layer[[1]], after = object$after)
  plot <- plot + new_coord
  plot
}

#' Load land polygons from OpenStreetMap.
#'
#' @usage get_land_polygons()
#' @export
get_land_polygons <- function() {
  if (is.null(cache_env$land_polygons)) {
    extdata_path <- system.file("extdata", package = "landr")
    polygons_shapefile <- file.path(extdata_path, "land-polygons-complete-4326", "land_polygons.shp")
    if (!file.exists(polygons_shapefile)) {
      polygons_archive <- file.path(extdata_path, "land-polygons-complete-4326.zip")
      message("Downloading shapefile from OpenStreetMap")
      download.file("https://osmdata.openstreetmap.de/download/land-polygons-complete-4326.zip", polygons_archive)
      unzip(polygons_archive, exdir = extdata_path)
    }
    message("Loading shapefile")
    land_polygons <- read_sf(polygons_shapefile)
    cache_env$land_polygons <- land_polygons
  }
  cache_env$land_polygons
}
