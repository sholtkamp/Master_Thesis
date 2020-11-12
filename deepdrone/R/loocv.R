loocv <- function(index){
  if(index == 1){
    these.tiles <- k_concatenate(c(tile_tensors[[2]],
                                   tile_tensors[[3]],
                                   tile_tensors[[4]],
                                   tile_tensors[[5]],
                                   tile_tensors[[6]]),
                                 1)
    these.masks <- k_concatenate(c(mask_tensors[[2]],
                                   mask_tensors[[3]],
                                   mask_tensors[[4]],
                                   mask_tensors[[5]],
                                   mask_tensors[[6]]),
                                 1)
  }
  
  if(index == 2){
    these.tiles <- k_concatenate(c(tile_tensors[[1]],
                                   tile_tensors[[3]],
                                   tile_tensors[[4]],
                                   tile_tensors[[5]],
                                   tile_tensors[[6]]),
                                 1)
    these.masks <- k_concatenate(c(mask_tensors[[1]],
                                   mask_tensors[[3]],
                                   mask_tensors[[4]],
                                   mask_tensors[[5]],
                                   mask_tensors[[6]]),
                                 1)
  }
  
  if(index == 3){
    these.tiles <- k_concatenate(c(tile_tensors[[1]],
                                   tile_tensors[[2]],
                                   tile_tensors[[4]],
                                   tile_tensors[[5]],
                                   tile_tensors[[6]]),
                                 1)
    these.masks <- k_concatenate(c(mask_tensors[[1]],
                                   mask_tensors[[2]],
                                   mask_tensors[[4]],
                                   mask_tensors[[5]],
                                   mask_tensors[[6]]),
                                 1)
  }
  
  if(index == 4){
    these.tiles <- k_concatenate(c(tile_tensors[[1]],
                                   tile_tensors[[2]],
                                   tile_tensors[[3]],
                                   tile_tensors[[5]],
                                   tile_tensors[[6]]),
                                 1)
    these.masks <- k_concatenate(c(mask_tensors[[1]],
                                   mask_tensors[[2]],
                                   mask_tensors[[3]],
                                   mask_tensors[[5]],
                                   mask_tensors[[6]]),
                                 1)
  }
  
  if(index == 5){
    these.tiles <- k_concatenate(c(tile_tensors[[1]],
                                   tile_tensors[[2]],
                                   tile_tensors[[3]],
                                   tile_tensors[[4]],
                                   tile_tensors[[6]]),
                                 1)
    these.masks <- k_concatenate(c(mask_tensors[[1]],
                                   mask_tensors[[2]],
                                   mask_tensors[[3]],
                                   mask_tensors[[4]],
                                   mask_tensors[[6]]),
                                 1)
  }
  
  if(index == 6){
    these.tiles <- k_concatenate(c(tile_tensors[[1]],
                                   tile_tensors[[2]],
                                   tile_tensors[[3]],
                                   tile_tensors[[4]],
                                   tile_tensors[[5]]),
                                 1)
    these.masks <- k_concatenate(c(mask_tensors[[1]],
                                   mask_tensors[[2]],
                                   mask_tensors[[3]],
                                   mask_tensors[[4]],
                                   mask_tensors[[5]]),
                                 1)
  }
  
  remainder <- list(these.tiles, these.masks)
  return(remainder)
}