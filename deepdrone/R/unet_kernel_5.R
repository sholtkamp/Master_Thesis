#' Unet Model
#'
#' @param input_shape 3D integer vector. Dimension of input data as c(x, y, dimensions/channels).
#' @param num_classes Integer. Number of classes for the classification. Needs to be >= 2.
#'
#' @source Based on the work on the keras vigniette found at https://github.com/rstudio/keras/blob/master/vignettes/examples/unet.R
#'
#' @return Returns defined model.
#' @export
#'
#' @examples
build_unet_kernel_5 <- function(input_shape = c(128, 128, 3), num_classes = 2){
  
  
  #---Input-------------------------------------------------------------------------------------
  inputs <- layer_input(name = "input_1", shape = input_shape)
  
  
  #---Downsampling------------------------------------------------------------------------------
  down1 <- inputs %>%
    layer_conv_2d(filters = 64, kernel_size = c(5, 5), name = "down_1", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(5, 5), name = "down_2", padding = "same") %>%
    layer_activation("relu")
  down1_pool <- down1 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  
  
  down2 <- down1_pool %>%
    layer_conv_2d(filters = 128, kernel_size = c(5, 5), name = "down_3", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(5, 5), name = "down_4", padding = "same") %>%
    layer_activation("relu")
  down2_pool <- down2 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  
  
  down3 <- down2_pool %>%
    layer_conv_2d(filters = 256, kernel_size = c(5, 5), name = "down_5", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(5, 5), name = "down_6", padding = "same") %>%
    layer_activation("relu")
  down3_pool <- down3 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  
  
  down4 <- down3_pool %>%
    layer_conv_2d(filters = 512, kernel_size = c(5, 5), name = "down_7", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(5, 5), name = "down_8", padding = "same") %>%
    layer_activation("relu")
  down4_pool <- down4 %>%
    layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))
  
  
  #---Center-----------------------------------------------------------------------------------
  center <- down4_pool %>%
    layer_spatial_dropout_2d(rate = 0.5) %>%
    layer_conv_2d(filters = 1024, kernel_size = c(5, 5), name = "center_1", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 1024, kernel_size = c(5, 5), name = "center_2", padding = "same") %>%
    layer_activation("relu")
  
  
  #---Upsampling--------------------------------------------------------------------------------
  up4 <- center %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down4, .), axis = 3)} %>%
    layer_conv_2d(filters = 512, kernel_size = c(5, 5), name = "up_8", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(5, 5), name = "up_7", padding = "same") %>%
    layer_activation("relu")
  
  up3 <- up4 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down3, .), axis = 3)} %>%
    layer_conv_2d(filters = 256, kernel_size = c(5, 5), name = "up_6", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(5, 5), name = "up_5", padding = "same") %>%
    layer_activation("relu") 
  
  up2 <- up3 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down2, .), axis = 3)} %>%
    layer_conv_2d(filters = 128, kernel_size = c(5, 5), name = "up_4", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(5, 5), name = "up_3", padding = "same") %>%
    layer_activation("relu") 
  
  up1 <- up2 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down1, .), axis = 3)} %>%
    layer_conv_2d(filters = 64, kernel_size = c(5, 5), name = "up_2", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(5, 5), name = "up_1", padding = "same") %>%
    layer_activation("relu") 
  
  
  #---Classification/Output---------------------------------------------------------------------
  classify <- layer_conv_2d(up1, filters = num_classes, kernel_size = c(1, 1), activation = "sigmoid")
  
  
  # Build specified model and assign it to variable
  model <- keras_model(
    inputs = inputs,
    outputs = classify
  )
  
  return(model)
}
