#' Threshold predicition probabilities to make class predictions
#'
#' @param threshold Value used as threshold to determine class. Value needs to be between 0.0 and 1.0. Reflects needed class probability for a pixel to be considered either class.
#' @param predictions Array of class predictions values returned from Keras' model based prediction function.
#' @param transparent Boolean. If TRUE pixels below threshold will be set to NA. This results in transparency for the exported raster file. Not recommended for further calculations.
#'
#' @return 2D array of binary values (0 or 1) for each pixel in the prediciton tiles.
#' @export
#'
#' @examples \dontrun{class_array <- threshold_results(prediciton_array, 0.95, TRUE)} 
threshold_results <- function(predictions, threshold, transparent){

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

            if(transparent == TRUE){
            # set class in result array to 0
            results[i, j, k] <- NA
            }
            
            else {
              results[i, j, k] <- 0
            }
          }

          # if threshold is exceeded
          else {

            # set class in result array to 1
            results[i, j, k] <- 1
          }
        }
      }
    }
  }

  return(results)
}







