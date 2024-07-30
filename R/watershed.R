#' Compute Watersheds from D8 Pointer and Pour Points
#'
#' This function calculates watershed areas based on the D8 pointer raster and pour points shapefile.
#'
#' @param d8_pntr A string specifying the file path to the D8 pointer raster.
#' @param pour_pts A string specifying the file path to the shapefile containing the snapped pour points.
#' @param output A string specifying the file path where the resulting watershed raster will be saved.
#'
#' @return The file path to the raster containing the computed watersheds.
#'
#' @examples
#' watershed_path <- compute_watersheds(
#'   d8_pntr = "path/to/D8pointer.tif",
#'   pour_pts = "path/to/snappedpp.shp",
#'   output = "path/to/rw_watersheds.tif"
#' )
#'
#' @export
compute_watersheds <- function(d8_pntr = system.file("extdata", "D8pointer.tif", package = "hydroModelling"), pour_pts = system.file("extdata", "snappedpp.shp", package = "hydroModelling"), output = tempfile(fileext = ".tif")) {
  whitebox::wbt_watershed(
    d8_pntr = d8_pntr,
    pour_pts = pour_pts,
    output = output
  )
  return(output)
}


#' Plot Watersheds, Hillshade, and Pour Points
#'
#' This function visualizes the watershed raster, hillshade raster, and pour points using tmap.
#'
#' @param hillshade_path A string specifying the file path to the hillshade raster.
#' @param watershed_path A string specifying the file path to the watershed raster.
#' @param pour_points_path A string specifying the file path to the shapefile containing the pour points.
#'
#' @return A tmap object that displays the hillshade, watershed, and pour points. This object can be printed or further customized.
#'
#' @export
plot_watersheds <- function(hillshade_path = system.file("extdata", "hillshade.tif", package = "hydroModelling"), watershed_path = system.file("extdata", "rw_watersheds.tif", package = "hydroModelling"), pour_points_path = system.file("extdata", "pourpoints.shp", package = "hydroModelling")) {
  hillshade <- terra::rast(hillshade_path)
  ws <- terra::rast(watershed_path)
  pp <- sf::st_read(pour_points_path)

  tmap::tm_shape(hillshade) +
    tmap::tm_raster(style = "cont", palette = "-Greys", legend.show = FALSE) +
    tmap::tm_shape(ws) +
    tmap::tm_raster(legend.show = TRUE, alpha = 0.5, style = "cat") +
    tmap::tm_shape(pp) +
    tmap::tm_dots(col = "red")
}
