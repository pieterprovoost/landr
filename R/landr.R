cache_env <- new.env(parent = emptyenv())
options(timeout = max(600, getOption("timeout")))

#' @title Add land polygons to ggplot2 maps
#'
#' @description
#' This package adds land polygons from OpenStreetMap to ggplot2 maps.
#'
#' @docType package
#' @name landr
#' @import ggplot2 sf memoise glue
#' @author Pieter Provoost, \email{pieterprovoost@gmail.com}
NULL

#' This geom adds land polygons from OpenStreetMap to ggplot2 maps.
#'
#' @usage geom_landr(after = 0, simplified = FALSE)
#' @param simplified return simplified polygons.
#' @param after index of the layer after which the land polygons should be added. Set to 0 to add at the bottom.
#' @export
geom_landr <- function(simplified = FALSE, after = 0, ...) {
  structure(list(after = after, simplified = simplified, args = list(...)), class = "landr")
}

get_clip <- function(bbox, crs, simplified) {
  sf_use_s2(FALSE)
  bbox_sf <- bbox %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_as_sf(crs = crs)
  cropped <- st_crop(st_transform(get_land_polygons(simplified), crs = crs), bbox_sf)
  cropped
}

get_clip_cached <- memoise::memoise(get_clip)

#' @export
ggplot_add.landr <- function(object, plot, object_name) {
  buffer <- 0.1
  b <- ggplot_build(plot)
  panel_params <- b$layout$panel_params[[1]]
  if ("x_range" %in% names(panel_params)) {
    xrange <- panel_params$x_range
    yrange <- panel_params$y_range
  } else if ("x.range" %in% names(panel_params)) {
    xrange <- panel_params$x.range
    yrange <- panel_params$y.range
  } else {
    stop("Could not find panel coordinate ranges")
  }
  dx <- xrange[2] - xrange[1]
  dy <- yrange[2] - yrange[1]
  bbox <- c(xmin = xrange[1] - dx * buffer, xmax = xrange[2] + dx * buffer, ymin = yrange[1] - dy * buffer, ymax = yrange[2] + dy * buffer)
  crs <- ifelse(is.null(b$layout$coord$crs), 4326, b$layout$coord$crs)
  clip <- get_clip_cached(bbox, crs, object$simplified)
  object$args$data <- clip
  layer <- do.call(geom_sf, object$args)
  plot$layers <- append(plot$layers, layer[[1]], after = object$after)
  plot <- plot + coord_sf(xlim = xrange, ylim = yrange, expand = FALSE, crs = crs)
  plot
}

#' Load land polygons from OpenStreetMap.
#'
#' @usage get_land_polygons(simplified = FALSE)
#' @param simplified return simplified polygons.
#' @export
get_land_polygons <- function(simplified = FALSE) {
  if (as.logical(simplified)) {
    folder_name <- "simplified-land-polygons-complete-3857"
    file_name <- "simplified_land_polygons.shp"
    object_name <- "simplified_land_polygons"
  } else {
    folder_name <- "land-polygons-complete-4326"
    file_name <- "land_polygons.shp"
    object_name <- "land_polygons"
  }
  if (!exists(object_name, cache_env)) {
    extdata_path <- system.file("extdata", package = "landr")
    polygons_shapefile <- file.path(extdata_path, folder_name, file_name)
    if (!file.exists(polygons_shapefile)) {
      polygons_archive <- file.path(extdata_path, glue("{folder_name}.zip"))
      message("Downloading shapefile from OpenStreetMap")
      download.file(glue("https://osmdata.openstreetmap.de/download/{folder_name}.zip"), polygons_archive)
      unzip(polygons_archive, exdir = extdata_path)
    }
    message("Loading shapefile")
    land_polygons <- read_sf(polygons_shapefile)
    assign(object_name, land_polygons, envir = cache_env)
  }
  polygons <- get(object_name, cache_env)
  if (is.numeric(simplified)) {
    polygons <- polygons %>% st_simplify(simplified, preserveTopology = TRUE)
  }
  polygons
}
