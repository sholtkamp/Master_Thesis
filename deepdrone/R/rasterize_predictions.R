#' Rasterize merged prediction tiles
#'
#' @param area_extent Extent. Class of raster package. Extent extracted from target area beforehand.
#' @param target_crs_pre CRS. Class of raster package. CRS of target area before reprojection.
#' @param target_crs_post CRS. Class of Raster package. CRS to reproject to.
#' @param merged_predictions Array. Array containing merged prediction tiles.
#'
#' @return Merged prediction tiles with georeferencing.
#' @export
#'
#' @examples \dontrun{rasterize_predictions(target_extent, 
#'                                          target_crs_pre, 
#'                                          target_crs_post, 
#'                                          binary_predictions_merged)}
rasterize_predictions <- function(area_extent, target_crs_pre, target_crs_post, merged_predictions){

  map_raster <- raster(merged_predictions)
  map_raster@extent <- area_extent
  map_raster@crs <- target_crs_pre
  map_raster <- projectRaster(from = map_raster, crs = target_crs_post)
  
  return(map_raster)
}
