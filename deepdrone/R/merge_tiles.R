#' Build a prediciton map for the complete test area
#'
#' @param prediction_tiles (thresholded) Output of a prediciton made using the Keras model
#' @param columns Number of columns/tiles on x-axis. Used to break rows of input tiles
#'
#' @return 2D array of predicted class values
#' @export
#'
#' @examples \dontrun{result_map <- merge_tiles(test_data[ , , ], 17)}
merge_tiles <- function(prediction_tiles, columns, overlap){
  
  # determine tile resolution of map from input
  x_resolution <- dim(prediction_tiles)[2]
  y_resolution <- dim(prediction_tiles)[3]
  
  # calculate number of rows given number of tiles and columns
  rows <- (dim(prediction_tiles)[1] / columns)
  
  # initiate array with dimensions of test area to fill with prediciton data
  map_array <- array(dim = c((rows * x_resolution), (columns * y_resolution)))
  
  # initiate counters for looping
  col_count <- 0
  row_count <- 0
  row_mod <- 0
  
  
  # for each prediciton tile
  for(i in 1:dim(prediction_tiles)[1]){
    
    # for each x
    for(j in 1:dim(prediction_tiles)[2]){
      # determine current x coordinate
      x_coor <- j + (col_count * x_resolution)
      
      # for each y
      for(k in 1:dim(prediction_tiles)[3]){
        # determine current y coordinate
        y_coor <- k + (row_count * y_resolution)
        
        # insert predicted value
        map_array[x_coor, y_coor] <- prediction_tiles[i, j, k]
      }
    }
    
    # increase row_count after each loop
    row_count <- (row_count + 1)
    
    # if the maximum number of tiles per row is reached
    if(row_count == columns){
      # reset position in row count
      row_count <- 0
      
      # increase column count
      col_count <- (col_count + 1)
    }
  }
  
  if(overlap == TRUE){
    
  y_drops <- seq(from = dim(prediction_tiles)[2], to = dim(map_array)[1] - (1 * dim(prediction_tiles)[2]), by = dim(prediction_tiles)[2])
  y_drops_l <- y_drops - 1
  y_drops_r <- y_drops + 1
  y_drops_l2 <- y_drops - 2
  y_drops_r2 <- y_drops + 2
  
  x_drops <- seq(from = dim(prediction_tiles)[2], to = dim(map_array)[2] - (1 * dim(prediction_tiles)[2]), by = dim(prediction_tiles)[2])
  x_drops_l <- x_drops - 1
  x_drops_r <- x_drops + 1
  x_drops_l2 <- x_drops - 2
  x_drops_r2 <- x_drops + 2
  
  map_array <- map_array[-c(y_drops, y_drops_l, y_drops_r, y_drops_l2, y_drops_r2),
                         -c(x_drops, x_drops_l, x_drops_r, x_drops_l2, x_drops_r2)]
  }
  
  
  
  return(map_array)
}
