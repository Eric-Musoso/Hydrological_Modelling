#' Compute D8 Flow Accumulation and D8 Pointers
#'
#' This function calculates the D8 flow accumulation and D8 pointers for a given digital elevation model (DEM) and filled DEM.
#'
#' The D8 flow accumulation represents the total amount of flow that accumulates to each cell in the raster grid based on the D8 flow model,
#' which directs flow to one of the eight neighboring cells. The D8 pointer raster indicates the direction of flow from each cell.
#'
#' @param dem_path A string specifying the file path to the input DEM raster. This DEM is used for calculating the D8 pointers.
#' @param filled_hillshade_path A string specifying the file path to the input filled DEM raster, used for calculating the D8 flow accumulation.
#' @param flow_acc_output_path A string specifying the file path where the resulting D8 flow accumulation raster will be saved.
#' @param pointer_output_path A string specifying the file path where the resulting D8 pointer raster will be saved.
#'
#' @return A list with two elements:
#' \item{flow_acc_output}{A string representing the file path to the D8 flow accumulation raster.}
#' \item{pointer_output}{A string representing the file path to the D8 pointer raster.}
#'
#' @examples
#' result <- flow_acc_and_pointer(
#'   dem_path = "path/to/dem.tif",
#'   filled_hillshade_path = "path/to/filled_dem.tif",
#'   flow_acc_output_path = "path/to/flow_accumulation.tif",
#'   pointer_output_path = "path/to/d8_pointer.tif"
#' )
#'
#' @export
flow_acc_and_pointer <- function(dem_path = system.file("extdata", "breached_hillshade.tif", package = "hydroModelling"),
                                 filled_hillshade_path = system.file("extdata", "filled_hillshade.tif", package = "hydroModelling"),
                                 flow_acc_output_path = tempfile(fileext = ".tif"),
                                 pointer_output_path = tempfile(fileext = ".tif")) {

  # Perform D8 Flow Accumulation
  whitebox::wbt_d8_flow_accumulation(
    input = filled_hillshade_path,
    output = flow_acc_output_path
  )

  # Perform D8 Pointer Calculation
  whitebox::wbt_d8_pointer(
    dem = dem_path,
    output = pointer_output_path
  )

  return(list(flow_acc_output = flow_acc_output_path, pointer_output = pointer_output_path))
}


#' Extract Streams from Flow Accumulation Raster
#'
#' This function extracts streams from a flow accumulation raster using a specified threshold. The streams are extracted using the whitebox package.
#'
#' @param flow_accum_path A string specifying the file path to the flow accumulation raster. Default is a file located in the extdata directory of the package.
#' @param output_path A string specifying the file path where the extracted streams raster will be saved. Default is a temporary file.
#' @param threshold A numeric value representing the threshold for stream extraction. Cells in the flow accumulation raster with values above this threshold are considered as streams. Default is 6000.
#'
#' @return A string specifying the file path to the extracted streams raster.
#' @importFrom whitebox wbt_extract_streams
#' @export
extract_streams <- function(flow_accum_path = system.file("extdata", "D8FA.tif", package = "hydroModelling"),
                            output_path = tempfile(fileext = ".tif"),
                            threshold = 6000) {
  whitebox::wbt_extract_streams(
    flow_accum = flow_accum_path,
    output = output_path,
    threshold = threshold
  )
  return(output_path)
}


#' Snap Pour Points to Stream Raster
#'
#' This function snaps pour points to the nearest stream in a raster format. It uses the WhiteboxTools
#' to perform the snapping operation. The input pour points are adjusted to the nearest stream within
#' a specified snapping distance.
#'
#' @param pour_pts_path A character string specifying the path to the pour points shapefile. Default is
#'   the example shapefile provided by the \pkg{hydroModelling} package.
#' @param streams_path A character string specifying the path to the raster file containing stream
#'   information. Default is the example raster file provided by the \pkg{hydroModelling} package.
#' @param output_path A character string specifying the path where the snapped pour points raster file
#'   will be saved. Default is a temporary file with the `.tif` extension.
#' @param snap_dist A numeric value specifying the snapping distance. Pour points will be moved to the
#'   nearest stream within this distance. Default is 0.0005.
#'
#' @return A character string with the path to the snapped pour points raster file.
#'
#' @import whitebox
#' @export
#'
#' @examples
#' # Example usage
#' snapped_pour_points <- snap_pour_points(
#'   pour_pts_path = system.file("extdata", "pourpoints.shp", package = "hydroModelling"),
#'   streams_path = system.file("extdata", "raster_streams.tif", package = "hydroModelling"),
#'   snap_dist = 0.001
#' )
#' print(snapped_pour_points)
snap_pour_points <- function(pour_pts_path = system.file("extdata", "pourpoints.shp", package = "hydroModelling"),
                             streams_path = system.file("extdata", "raster_streams.tif", package = "hydroModelling"),
                             output_path = tempfile(fileext = ".tif"),
                             snap_dist = 0.0005) {
  whitebox::wbt_jenson_snap_pour_points(
    pour_pts = pour_pts_path,
    streams = streams_path,
    output = output_path,
    snap_dist = snap_dist
  )
  return(output_path)
}


#' Plot Stream Networks and Pour Points
#'
#' This function creates a map visualization of stream networks and pour points using tmap.
#' It reads in a raster of stream networks and a shapefile of pour points, then plots them on the same map.
#'
#' @param streams_path A string specifying the file path to the raster file containing the stream network. This raster will be visualized with a blue color palette.
#' @param snapped_pp_path A string specifying the file path to the shapefile containing the pour points. These points will be overlaid on the stream network in red.
#'
#' @return A tmap object that displays the stream network and pour points. This object can be printed or further customized.
#'
#' @export
#'
plot_streams_and_pour_points <- function(streams_path = system.file("extdata", "raster_streams.tif", package = "hydroModelling"),
                                         snapped_pp_path = system.file("extdata", "snappedpp.shp", package = "hydroModelling") ) {
  streams <- terra::rast(streams_path)
  pp <- sf::st_read(snapped_pp_path)

  tmap::tm_shape(streams) +
    tmap::tm_raster(legend.show = TRUE, palette = "Blues") +
    tmap::tm_shape(pp) +
    tmap::tm_dots(col = "red")
}
