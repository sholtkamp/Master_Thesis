#' Threshold predicition probabilities to make class predictions
#'
#' @param threshold Value used as threshold to determine class. Value needs to be between 0.0 and 1.0. Reflects needed class probability for a pixel to be considered either class.
#' @param predictions Array of class predictions values returned from Keras' model based prediction function.
#'
#' @return 2D array of binary values (0 or 1) for each pixel in the prediciton tiles.
#' @export
#'
#' @examples \dontrun{class_array <- binary_threshold_results(prediciton_array, 0.95)} This thresholds an array of predicitons with a minimum probability of 95% for class to. This can be interpreted as, for example, "A pixel needs to have at least a 95% probability of beeing a tree to be considered as one for the final result".
threshold_results <- function(predictions, threshold){

  # initiate array to fill with thresholded class values
  results <- array(dim = c(dim(predictions)[1], dim(predictions)[2], dim(predictions)[3]))

  # for each tile in predicitions
  for(i in 1:dim(predictions)[1]) {

    # for each x coordinate
    for(j in 1:dim(predictions)[2]){

      # for each y coordiante
      for(k in 1:dim(predictions)[3]) {

        # for each class in prediction
        for(l in 1:dim(predictions)[4]) {

          # if class 2 is less probable than given threshold
          if(predictions[i, j, k, 1] <= threshold){

            # set class in result array to 0
            results[i, j, k] = 0
          }

          # if threshold is exceeded
          else {

            # set class in result array to 1
            results[i, j, k] = 1
          }
        }
      }
    }
  }

  return(results)
}


#' Build a prediciton map for the complete test area
#'
#' @param prediction_tiles (thresholded) Output of a prediciton made using the Keras model
#' @param columns Number of columns/tiles on x-axis. Used to break rows of input tiles
#'
#' @return 2D array of predicted class values
#' @export
#'
#' @examples \dontrun{result_map <- build_map(test_data[ , , ], 17)}
superset_results <- function(prediction_tiles, columns){

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

  return(map_array)
}
