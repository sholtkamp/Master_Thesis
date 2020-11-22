#' Threshold predicition probabilities to make class predictions
#'
#' @param threshold Value used as threshold to determine class. Value needs to be between 0.0 and 1.0. Reflects needed class probability for a pixel to be considered either class.
#' @param predictions Array of class predictions values returned from Keras' model based prediction function.
#'
#' @return 2D array of binary values (0 or 1) for each pixel in the prediciton tiles.
#' @export
#'
#' @examples \dontrun{class_array <- binary_threshold_results(prediciton_array, 0.95)} This thresholds an array of predicitons with a minimum probability of 95% for class to. This can be interpreted as, for example, "A pixel needs to have at least a 95% probability of beeing a tree to be considered as one for the final result".
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





raster_maps <- function(data, map){
  
  ext <- data[[6]]
  crs <- data[[7]]
  map_raster <- raster(map)
  map_raster@extent <- extent(480321.8,
                              480396.5,
                              5796794,
                              5796879)
  map_raster@crs <- crs("+proj=utm +zone=32 +datum=WGS84 +init=EPSG:32632 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  map_raster <- projectRaster(from = map_raster, crs = "+proj=longlat +datum=WGS84 +no_defs +zone=32 +init=EPSG:32632")
  
  return(map_raster)
}


filter_results_binary <- function(map_class_1, map_class_2){
  for (i in 1:dim(map_class_1)[1]){
    for (j in 1:dim(map_class_1)[2]){
      if(map_class_1[i, j] < map_class_2[i, j]){
        map_class_1[i, j] <- 0
      }
      else {
        map_class_1[i, j] <- 1
      }
    }
  }
  
  return(map_class_1)
}


