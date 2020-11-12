loocv <- function(tile_tensors, mask_tensors, index){
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

evaluate_loocv <- function(evaluations){
  
  sum_loss <- 0
  sum_dice <- 0
  sum_acc <- 0
  
  for(i in 1:length(evaluations)){
    sum_loss <- sum_loss + evaluations[[i]]$loss
    sum_dice <- sum_dice + evaluations[[i]]$dice
    sum_acc <- sum_acc + evaluations[[i]]$acc
  }
  
  avg_loss <- sum_loss / length(evaluations)
  avg_dice <- sum_dice / length(evaluations)
  avg_acc <- sum_acc / length(evaluations)
  
  return(list("avg_loss" = avg_loss, "avg_dice" = avg_dice, "avg_acc" = avg_acc))
}