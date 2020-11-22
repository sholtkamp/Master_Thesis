#' Balance binary class data using oversampling
#'
#' @param paradigm String. Either "pixel" or "image" can be passed as arguments. "image" is default value. Selects how the data set will be balanced.
#' @param tile_data Double array. Containing numerical representation of raster data.
#' @param mask_data Int array. Containing class values for tile data ranging from 0 to 1.
#' @param aim Double. Ranges between 0 and 1. Represents percentage to which the data set will be balanced.
#'
#' @return Returns balanced dataset.
#' @export
#'
#' @examples \dontrun{balanced_dataset <- balance_data("pixel", tile_data, mask_data, 0.5)}
balance_data <- function(paradigm = "pixel", tile_data, mask_data, aim){

  aim <- as.double(aim)
  tile_data <- data[[1]]
  mask_data <- data[[2]]

  cat("\n")
  print("Balancing data")

  # if paradigm is selected as pixel, the data set will be balanced on basis of pixels and their class affiliation
  if(paradigm == "pixel"){

    # calculate current balance between the two classes
    balance_score <- sum(mask_data != 0) / length(mask_data)
    print(paste0("Initial balance_score: ", round((balance_score* 100), 2), "%"))

    # initiate list to store ids of masks with values >= aim
    positive_ids <- list()

    # loop over all masks
    for(i in 1:dim(mask_data)[1]){
      # calculate balance of asingle mask
      single_score <- sum(mask_data[i, , , ] != 0) / (dim(mask_data)[2] * dim(mask_data)[3])

      # if score is higher than aim save id
      if(single_score > aim){
        positive_ids <- append(positive_ids, i)
      }
    }

    # prepare arrays for tiles and masks with sufficient score
    pos_tiles <- array(dim = c(length(positive_ids), dim(tile_data)[2], dim(tile_data)[3], dim(tile_data)[4]))
    pos_masks <- array(dim = c(length(positive_ids), dim(mask_data)[2], dim(mask_data)[3], dim(mask_data)[4]))

    # fill arrays
    index <- 0
    for(i in 1:dim(tile_data)[1]){
      if(i %in% positive_ids){
        pos_tiles[index, , , ] <- tile_data[i, , , ]
        pos_masks[index, , , ] <- mask_data[i, , , ]
        index <- index + 1
      }
    }

    # while balance_score is lower than desired concatenate masks and tiles to input data to increase score
    while(balance_score < aim){
      counter_2 <- 0

      tile_data <- abind(tile_data, pos_tiles, along = 1)
      mask_data <- abind(mask_data, pos_masks, along = 1)

      mask_data[is.na(mask_data)] <- 0

      balance_score <- sum(mask_data != 0) / length(mask_data)
      print(paste0("Current balance_score: ", round((balance_score* 100), 2), "%"))
    }
  }

  #----------------------------------------------------------------------------------

  if(paradigm == "image"){

    # initiate list to store ids of masks with values >= aim
    positive_ids <- list()

    # fill list with positive ids
    for(i in 1:dim(mask_data)[1]){
      if(any(1 %in% mask_data[i, , , ])){
        positive_ids <- append(positive_ids, i)
      }
    }

    # calculate current balance between the two classes
    balance_score <- length(positive_ids) / dim(mask_data)[1]
    print(paste0("Initial balance_score: ", round((balance_score* 100), 2), "%"))

    # prepare arrays for tiles and masks with sufficient score
    pos_tiles <- array(dim = c(length(positive_ids), dim(tile_data)[2], dim(tile_data)[3], dim(tile_data)[4]))
    pos_masks <- array(dim = c(length(positive_ids), dim(mask_data)[2], dim(mask_data)[3], dim(mask_data)[4]))

    # fill arrays
    index <- 0
    for(i in 1:dim(tile_data)[1]){
      if(i %in% positive_ids){
        pos_tiles[index, , , ] <- tile_data[i, , , ]
        pos_masks[index, , , ] <- mask_data[i, , , ]
        index <- index + 1
      }
    }

    # while balance_score is lower than desired concatenate masks and tiles to input data to increase score
    while(balance_score < aim){
      counter_2 <- 0

      tile_data <- abind(tile_data, pos_tiles, along = 1)
      mask_data <- abind(mask_data, pos_masks, along = 1)

      mask_data[is.na(mask_data)] <- 0

      for(i in 1:dim(mask_data)[1]){
        if(any(1 %in% mask_data[i, , , ])){
          counter_2 <- counter_2 + 1
        }
      }

      balance_score <- counter_2 / dim(mask_data)[1]
      print(paste0("Current balance_score: ", round((balance_score* 100), 2), "%"))
    }
  }

  print("Data balanced")

  # make list of arrays to return
  balanced_data <- list("tile_data" = tile_data, "mask_data" = mask_data)


  return(balanced_data)
}
