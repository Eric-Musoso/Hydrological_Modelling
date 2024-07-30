#' Load DEM data and set CRS
#'
#' @param dem_path The path to the DEM file.
#' @param crs_code The CRS code to set for the DEM (default is "EPSG:4326").
#'
#' @return A SpatRaster object with the DEM data.
#' @importFrom terra rast crs
#' @export
#'
#'
load_and_set_crs <- function(dem_path = system.file("extdata", "demRw.tif", package = "hydroModelling"), crs_code = "EPSG:4326") {
  dem <- terra::rast(dem_path)

  tryCatch({
    terra::crs(dem) <- crs_code
  }, error = function(e) {
    message("Error setting CRS: ", e)
    message("Make sure PROJ and GDAL are correctly installed and PROJ_LIB is set.")
  })

  if (is.na(terra::crs(dem))) {
    stop("Failed to set CRS. Please ensure proj.db is accessible and correctly configured.")
  } else {
    message("CRS successfully set.")
  }

  return(dem)
}


#' Apply threshold to DEM
#'
#' This function applies a threshold to the DEM data, setting values below the threshold to NA.
#'
#' @param dem A SpatRaster object with the DEM data.
#' @param threshold The threshold value (default is 923.667).
#' @return A SpatRaster object with the threshold applied.
#' @export
apply_threshold <- function(dem, threshold = 923.667) {
  dem <- load_and_set_crs()
  dem[dem < threshold] <- NA
  return(dem)
}


#' Plot DEM
#'
#' This function plots the DEM data using tmap.
#'
#' @param dem A SpatRaster object with the DEM data.
#' @export

plot_dem <- function(dem = apply_threshold()) {
  tmap::tm_shape(dem) +
    tmap::tm_raster(style = "cont", palette = "PuOr", legend.show = TRUE) +
    tmap::tm_scale_bar()
}


#' Write raster with CRS
#'
#' This function writes the raster data with CRS to a specified path.
#'
#' @param dem A SpatRaster object with the DEM data.
#' @param output_path The path to save the raster file.
#' @export
#'
write_raster_with_crs <- function(dem = load_and_set_crs(), output_path = "D:/Munster/Data Science w R/finalProject/demRw_30m_crs.tif") {
  terra::writeRaster(dem, output_path, overwrite = TRUE)
}
