#' Wrapper function used to load tile and mask data for project.
#'
#' @param tile_path String. File path to tile data.
#' @param mask_path String. File path to mask data.
#' @param dimensions Integer. Vector of tile dimensions, e.g. c(128,128)
#' @param ch_t Integer. Number of channels in tile data.
#' @param ch_m Integer. Number of channels in mask data.
#' @references read_tiles()
#' 
#' @return Returns array containing project data
#' @export
#'
#' @examples \dontrun{load_project_data(tile_path = "F:/Master_Thesis/data/tiles/", mask_path = "F:/Master_Thesis/data/masks/", ch_t = 6, ch_m = 1, dimensions = c(128, 128), format = ".tif")}
load_project_data <- function(tile_path, mask_path, dimensions, ch_t, ch_m, format){
  
  # read raster data
  tile_data <- read_tiles(tile_path, dimensions, ch_t, ".tif")
  mask_data <- read_tiles(mask_path, dimensions, ch_m, ".tif")
 
  # fill NAs with 0
  mask_data[is.na(mask_data)] <- 0
  
  # make list of arrays to return
  data <- list("tile_data" = tile_data, "mask_data" = mask_data)
  
  return(data)
}
