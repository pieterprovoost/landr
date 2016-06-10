#' Determine how far inland a point is located.
#'
#' @param lon longitude
#' @param lat latitude
#' @return a vector of distances in decimal degrees
#' @export
land <- function(lon, lat) {

  distances <- NULL

  for (i in 1:max(length(lon), length(lat))) {
    point <- readWKT(sprintf("POINT(%s %s)", lon[i], lat[i]))
    suppressWarnings(
      d <- gDistance(point, shapes)
    )
    distances <- c(distances, d)
  }

  return(distances)

}
