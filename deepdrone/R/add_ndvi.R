#' Add NDVI data calculated from tiles to data set at last dimension
#'
#' @param data Array. Image data tiles. Needs to contain NIR and RED bands.
#' @param index_red_band Integer. Number of RED band.
#' @param index_nir_band Integer. Number of NIR band.
#'
#' @return Returns array with added NDVI dimension.
#' @export
#'
#' @examples \dontrun{add_ndvi <- add_ndvi(image_data, 1, 3)}
add_ndvi <- function(data, index_red_band, index_nir_band){

  cat("\n")
  print("Calculating NDVI")

  # initiate array to hold all tile data plus ndvi
  temp_tiles <- array(dim = c(dim(data)[1], dim(data)[2],
                              dim(data)[3], (dim(data)[4] + 1)))

  # for each tile
  for(i in 1:dim(temp_tiles)[1]){

    # for each x in tile
    for(j in 1:dim(temp_tiles)[2]){

      # for each y in tile and x
      for(k in 1:dim(temp_tiles)[3]){

        # for each layer in tile
        for(l in 1:dim(temp_tiles)[4]){

          # for all original layers in input copy data to array
          if(l < dim(temp_tiles)[4]){
            temp_tiles[i, j, k, l] <- data[i, j, k, l]
          }

          # if l is the additional layer computer ndvi and put it into array
          if(l == dim(temp_tiles)[4]) {
            temp_tiles[i, j, k, l] <- (data[i, j, k, index_nir_band] - data[i, j, k, index_red_band]) /
              (data[i, j, k, index_nir_band] + data[i, j, k, index_red_band]) # calculate ndvi using standard formula
          }
        }
      }
    }
  }

  # filter NAs by replacing them with 0
  temp_tiles[is.na(temp_tiles[ , , , 7])] <- 0

  print("NDVI calculated and added to data.")


  return(temp_tiles)
}
