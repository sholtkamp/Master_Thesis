build_unet_shallow <- function(input_shape = c(128, 128, 3), num_classes = 2){
  
  
  #---Input-------------------------------------------------------------------------------------
inputs <- layer_input(name = "input_1", shape = input_shape)

down4 <- inputs %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), name = "down_7", padding = "same") %>%
  layer_activation("relu") %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), name = "down_8", padding = "same") %>%
  layer_activation("relu")
down4_pool <- down4 %>%
  layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2))


#---Center-----------------------------------------------------------------------------------
center <- down4_pool %>%
  layer_spatial_dropout_2d(rate = 0.5) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), name = "center_1", padding = "same") %>%
  layer_activation("relu") %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), name = "center_2", padding = "same") %>%
  layer_activation("relu")


#---Upsampling--------------------------------------------------------------------------------
up4 <- center %>%
  layer_upsampling_2d(size = c(2, 2)) %>%
  {layer_concatenate(inputs = list(down4, .), axis = 3)} %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), name = "up_8", padding = "same") %>%
  layer_activation("relu") %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), name = "up_7", padding = "same") %>%
  layer_activation("relu")

classify <- layer_conv_2d(up4, filters = num_classes, kernel_size = c(1, 1), activation = "sigmoid")


# Build specified model and assign it to variable
model <- keras_model(
  inputs = inputs,
  outputs = classify
)

return(model)
}
