#' Generate Hillshade from DEM
#'
#' This function generates a hillshade raster from a Digital Elevation Model (DEM). It first applies a threshold to the DEM, then generates the hillshade raster using the whitebox package.
#'
#' @param dem A SpatRaster object representing the Digital Elevation Model (DEM).
#' @param output_path A string specifying the file path where the generated hillshade raster will be saved. Default is a temporary file path.
#' @param azimuth A numeric value representing the azimuth angle (in degrees) for the sun's position, used in creating the hillshade effect. Default is 115 degrees.
#'
#' @return A string specifying the file path to the generated hillshade raster.
#' @export
#' @examples
#' \dontrun{
#' dem <- terra::rast("path/to/dem.tif")
#' generate_hillshade(dem, output_path = "output/hillshade.tif", azimuth = 115)
#' }
#'

generate_hillshade <- function(dem, output_path = tempfile(fileext = ".tif"), azimuth = 115) {
  dem_thresholded <- apply_threshold(dem)
  thresholded_dem_path <- tempfile(fileext = ".tif")
  terra::writeRaster(dem_thresholded, thresholded_dem_path, overwrite = TRUE)
  whitebox::wbt_hillshade(
    dem = thresholded_dem_path,
    output = output_path,
    azimuth = azimuth
  )
  return(output_path)
}

#' Plot Hillshade Raster
#'
#' This function generates a hillshade raster from a Digital Elevation Model (DEM) and plots it using the tmap package. The hillshade raster is first created using the generate_hillshade function, and then plotted.
#'
#' @param dem A SpatRaster object representing the Digital Elevation Model (DEM). This DEM will be used to generate the hillshade.
#' @param hillshade_path A string specifying the file path where the generated hillshade raster will be saved. Default is a file located in the extdata directory of the package.
#'
#' @return A tmap object representing the hillshade plot.
#' @importFrom terra rast
#' @importFrom tmap tm_shape tm_raster tm_scale_bar
#' @export
#'
#' @examples
#' \dontrun{
#' dem <- terra::rast("path/to/dem.tif")
#' plot_hillshade(dem, hillshade_path = "output/hillshade.tif")
#' }
#'
#'
plot_hillshade <- function(dem, hillshade_path = system.file("extdata", "hillshade.tif", package = "hydroModelling")) {
  generate_hillshade(dem, hillshade_path)
  hillshade <- terra::rast(hillshade_path)
  tmap::tm_shape(hillshade) +
    tmap::tm_raster(style = "cont", palette = "-Greys", legend.show = FALSE) +
    tmap::tm_scale_bar()
}


#' Fill Depressions in Hillshade DEM
#'
#' This function processes a hillshade DEM by breaching depressions and then filling them using the Wang and Liu method. The input hillshade DEM is taken from the extdata directory of the package. The function outputs a filled DEM that has had depressions breached and filled.
#'
#' @param hillshade_path The path to the input hillshade DEM file. Default is a file located in the extdata directory of the package.
#' @param dist The maximum search distance for breaching depressions. Default is 5.
#' @param fill Logical parameter to determine if the function should fill depressions. Default is TRUE.
#'
#' @return The path to the filled DEM file.
#' @importFrom whitebox wbt_breach_depressions_least_cost wbt_fill_depressions_wang_and_liu
#' @export
#'
#' @examples
#' \dontrun{
#'   filled_dem <- fill_hillshade(
#'     hillshade_path = system.file("extdata", "hillshade.tif", package = "hydroModelling"),
#'     dist = 10,
#'     fill = TRUE
#'   )
#'   print(filled_dem)
#' }
#'
#'
#'
fill_hillshade <- function(hillshade_path = system.file("extdata", "hillshade.tif", package = "hydroModelling"), dist = 5, fill = TRUE) {
  breached_dem_path <- tempfile(fileext = ".tif")
  filled_dem_path <- tempfile(fileext = ".tif")

  whitebox::wbt_breach_depressions_least_cost(
    dem = hillshade_path,
    output = breached_dem_path,
    dist = dist,
    fill = fill
  )

  whitebox::wbt_fill_depressions_wang_and_liu(
    dem = breached_dem_path,
    output = filled_dem_path
  )

  return(filled_dem_path)
}
