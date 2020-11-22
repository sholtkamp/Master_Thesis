#' Area-based leave one out cross validation. Select one area which is left out of the training process and used as evaluation target.
#'
#' @param tile_tensors List. Contains alls available tile tensors.
#' @param mask_tensors List. Contains all available maks tensors.
#' @param index Interger. Index of area to be left out in this iteration.
#'
#' @return Concatenated tensors of all areas except left out one.
#' @export
#'
#' @examples \dontrun{cross_validation_tensor_selection <- abloo_cv(train_tile_tensors, train_mask_tensors, 3)}
abloo_cv <- function(tile_tensors, mask_tensors, index){
  if(index == 1){
    these.tiles <- k_concatenate(c(tile_tensors[[2]],
                                   tile_tensors[[3]],
                                   tile_tensors[[4]],
                                   tile_tensors[[5]]),
                                 1)
    these.masks <- k_concatenate(c(mask_tensors[[2]],
                                   mask_tensors[[3]],
                                   mask_tensors[[4]],
                                   mask_tensors[[5]]),
                                 1)
  }
  
  if(index == 2){
    these.tiles <- k_concatenate(c(tile_tensors[[1]],
                                   tile_tensors[[3]],
                                   tile_tensors[[4]],
                                   tile_tensors[[5]]),
                                 1)
    these.masks <- k_concatenate(c(mask_tensors[[1]],
                                   mask_tensors[[3]],
                                   mask_tensors[[4]],
                                   mask_tensors[[5]]),
                                 1)
  }
  
  if(index == 3){
    these.tiles <- k_concatenate(c(tile_tensors[[1]],
                                   tile_tensors[[2]],
                                   tile_tensors[[4]],
                                   tile_tensors[[5]]),
                                 1)
    these.masks <- k_concatenate(c(mask_tensors[[1]],
                                   mask_tensors[[2]],
                                   mask_tensors[[4]],
                                   mask_tensors[[5]]),
                                 1)
  }
  
  if(index == 4){
    these.tiles <- k_concatenate(c(tile_tensors[[1]],
                                   tile_tensors[[2]],
                                   tile_tensors[[3]],
                                   tile_tensors[[5]]),
                                 1)
    these.masks <- k_concatenate(c(mask_tensors[[1]],
                                   mask_tensors[[2]],
                                   mask_tensors[[3]],
                                   mask_tensors[[5]]),
                                 1)
  }
  
  if(index == 5){
    these.tiles <- k_concatenate(c(tile_tensors[[1]],
                                   tile_tensors[[2]],
                                   tile_tensors[[3]],
                                   tile_tensors[[4]]),
                                 1)
    these.masks <- k_concatenate(c(mask_tensors[[1]],
                                   mask_tensors[[2]],
                                   mask_tensors[[3]],
                                   mask_tensors[[4]]),
                                 1)
  }

  
  remainder <- list(these.tiles, these.masks)
  return(remainder)
}

#' Avaluate model quality using area-based leave one out cross validation
#'
#' @param evaluations List. Contains evaluations gathered in area-based leave one out cross validation training
#'
#' @return List. Contains averaged metrics
#' @export
#'
#' @examples
evaluate_abloo_cv <- function(evaluations){
  
  # initiate variables to hold metrics
  sum_loss <- 0
  sum_dice <- 0
  sum_IoU <- 0
  sum_acc <- 0
  
  # sum up metrics
  for(i in 1:length(evaluations)){
    sum_loss <- sum_loss + evaluations[[i]]$loss
    sum_dice <- sum_dice + evaluations[[i]]$dice
    sum_IoU <- sum_IoU + evaluations[[i]]$IoU
    sum_acc <- sum_acc + evaluations[[i]]$acc
  }
  
  # average metrics
  avg_loss <- sum_loss / length(evaluations)
  avg_dice <- sum_dice / length(evaluations)
  avg_IoU <- sum_IoU / length(evaluations)
  avg_acc <- sum_acc / length(evaluations)
  
  return(list("avg_loss" = avg_loss, "avg_dice" = avg_dice, "avg_IoU" = avg_IoU, "avg_acc" = avg_acc))
}
