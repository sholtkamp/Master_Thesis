#' Apply a test/training split to data
#'
#' @param data Array. Tile data.
#' @param split Numeric value between 0 and 1. Represents train/test split
#'
#' @return Returns data with both tiles and masks split into train (index 1, 2) and test (3, 4) data.
#' @export
#'
#' @examples \dontrun{split_dataset <- split_data(data, 0.8)}
split_data <- function(data, split = 0.8){

  cat("\n")
  print("Splitting data:")

  tile_data <- data[[1]]
  mask_data <- data[[2]]

  dimensions <- c(dim(data[[1]])[2],
                  dim(data[[1]])[3],
                  dim(data[[1]])[4])


  # determine point in input data to split between train and test data set
  split_point <- floor(dim(tile_data)[1] * split)


  # initialize arrays to contain subsets
  train_data <- array(dim = c(split_point, dimensions[1], dimensions[2], dimensions[3]))
  train_mask <- array(dim = c(split_point, dimensions[1], dimensions[2], 1))

  test_data <- array(dim = c((dim(tile_data)[1] - split_point), dimensions[1], dimensions[2], dimensions[3]))
  test_mask <- array(dim = c((dim(mask_data)[1] - split_point), dimensions[1], dimensions[2], 1))


  # make traceable sample selection by sampling ids and building a vector from them
  sample_ids <- sample(1:(dim(tile_data)[1]), split_point)


  # initiate indices
  train_index = 1
  test_index = 1

  # loop over whole data and split tiles and masks into train and test data sets according to sampled ids
  for(i in 1 : (dim(tile_data)[1])){

    if(i %in% sample_ids){
      # case: id is a sampled id --> point of data belongs into training set
      train_data[train_index, , , ] <- tile_data[i, , , ]
      train_mask[train_index, , , ] <- mask_data[i, , , 1]

      train_index <- train_index + 1
    }

    else {
      # case: id is not a sampled id --> point of data belongs to test set
      test_data[test_index, , , ] <- tile_data[i, , , ]
      test_mask[test_index, , , ] <- mask_data[i, , , 1]

      test_index <- test_index + 1
    }

  }

  # make list of arrays to return
  split_data <- list("train_data" = train_data, "train_mask" = train_mask,
                     "test_data" = test_data, "test_mask" = test_mask)

  print("Dataset has been split.")


  return(split_data)
}
