#' Build tensors from input tile and mask data
#'
#' @param input The complete data array built with load_data() and the preprocessing functions.
#' @param bands Integer vector. Spectral bands the tensors will incorporate.
#' @param leading_ids Integer vector. IDs signifying new areas of interest. Needs to start with 1 and end with number of tiles + 1.
#'
#' @return
#' @export
#'
#' @examples \dontrun{tensors <- tensorize_data(data, c(4, 5, 6), c(1, 316, 328, 581, 617, 632))}
tensorize_data <- function(input = data, bands = c(4, 5, 6), leading_ids = c(1, 316, 328, 581, 617, 632)){

  # initiate lists to organize tensors and masks
  tile_tensors <- list()
  mask_tensors <- list()

  # for all areas
  for(i in 1:(length(leading_ids) - 1)){

    # set first and last tile ID
    lead <- leading_ids[i]
    tail <- (leading_ids[i + 1] - 1)

    # build tensor of tiles
    tile_tensor <- k_constant(input[[1]][lead:tail, , , bands])
    tile_tensors[[paste0("tiles_area_", i)]] <- tile_tensor

    # build tensor of masks
    mask_tensor <- k_constant(to_categorical(input[[2]][lead:tail, , ], num_classes = 2))
    mask_tensors[[paste0("masks_area_", i)]] <- mask_tensor
  }

  # merge lists into super list
  tiles_and_masks <- list("tile_tensors" = tile_tensors, "mask_tensors" = mask_tensors)

  return(tiles_and_masks)
}
