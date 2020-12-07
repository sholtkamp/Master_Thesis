#' Custom Model
#'
#' Build a customized, vgg16 and unet based model
#'
#' @source Partially based on the work of Christian Knoth at https://github.com/DaChro/cannons_at_marmots
#'
#' @param input_shape Dimensions of input. Standard: 128*128 resolution, 1 channel greyscale; for RGB use 3 channels
#' @param num_classes Number of classes. Standard: 2 for binary classification. Example: Tree vs. not tree
#'
#' @return Returns a Keras model
#'
#' @examples \dontrun{model <- build_vgg_unet()}
build_custom_model <- function(input_shape = c(128, 128, 3), num_classes = 2) {


  #---Input-------------------------------------------------------------------------------------
  inputs <- layer_input(name = "input_1", shape = input_shape)


  #---Downsampling------------------------------------------------------------------------------
  down1 <- inputs %>%
    layer_conv_2d(name = "down1_conv1", filters = 64, kernel_size = 3,
                  input_shape = c(128, 128, 1),
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_conv_2d(name = "down1_conv2", filters = 64, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling")
  down1_pool <- down1 %>%
    layer_max_pooling_2d(name = "down1_pool", pool_size = c(2, 2), strides = c(2, 2))


  down2 <- down1_pool %>%
    layer_conv_2d(name = "down2_conv1", filters = 128, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_conv_2d(name = "down2_conv2", filters = 128, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling")
  down2_pool <- down2 %>%
    layer_max_pooling_2d(name = "down2_pool", pool_size = c(2, 2), strides = c(2, 2)) %>%
    layer_dropout(0.2)


  down3 <- down2_pool %>%
    layer_conv_2d(name = "down3_conv1", filters = 256, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_conv_2d(name = "down3_conv2", filters = 256, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling")
  down3_pool <- down3 %>%
    layer_max_pooling_2d(name = "down3_pool", pool_size = c(2, 2), strides = c(2, 2))


  down4 <- down3_pool %>%
    layer_conv_2d(name = "down4_conv1", filters = 512, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_conv_2d(name = "down4_conv2", filters = 512, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling")
  down4_pool <- down4 %>%
    layer_max_pooling_2d(name = "down4_pool", pool_size = c(2, 2), strides = c(2, 2))


  #---Center-----------------------------------------------------------------------------------
  center <- down4_pool %>%
    layer_dropout(0.2)


  #---Upsampling--------------------------------------------------------------------------------
  up4 <- center %>%
    layer_conv_2d(name = "up4_conv1", filters = 512, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_conv_2d(name = "up4_conv2", filters = 512, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_batch_normalization() %>%
    layer_conv_2d_transpose(name = "up4_upconv_1", filters = 128, kernel_size = 2, strides = c(2, 2),
                            padding = "same", data_format = "channels_last", activation = "linear") %>%
    {layer_concatenate(name = "up4_conc1", inputs = list(down4, .))}


  up3 <- up4 %>%
    layer_conv_2d(name = "up3_conv1", filters = 256, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_conv_2d(name = "up3_conv2", filters = 256, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_batch_normalization() %>%
    layer_conv_2d_transpose(name = "up3_upconv_1", filters = 128, kernel_size = 2, strides = c(2, 2),
                            padding = "same", data_format = "channels_last", activation = "linear") %>%
    {layer_concatenate(name = "up3_conc1", inputs = list(down3, .))}


  up2 <- up3 %>%
    layer_conv_2d(name = "up2_conv1", filters = 128, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_conv_2d(name = "up2_conv2", filters = 128, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_batch_normalization() %>%
    layer_conv_2d_transpose(name = "up2_upconv1", filters = 128, kernel_size = 2, strides = c(2, 2),
                            padding = "same", data_format = "channels_last", activation = "linear") %>%
    {layer_concatenate(name = "up2_conc1", inputs = list(down2, .))} %>%
    layer_dropout(0.2)


  up1 <- up2 %>%
    layer_conv_2d(name = "up1_conv1", filters = 64, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_conv_2d(name = "up1_conv2", filters = 64, kernel_size = 3,
                  padding = "same", data_format = "channels_last",
                  activation = "relu", kernel_initializer = "VarianceScaling") %>%
    layer_batch_normalization() %>%
    layer_conv_2d_transpose(name = "up1_upconv1", filters = 128, kernel_size = 2, strides = c(2, 2),
                            padding = "same", data_format = "channels_last", activation = "linear") %>%
    {layer_concatenate(name = "up1_conc1", inputs = list(down1, .))}


  #---Classification/Output---------------------------------------------------------------------
  classify <- layer_conv_2d(up1, filters = num_classes, kernel_size = c(1, 1), activation = "sigmoid")


  # Build specified model and assign it to variable
  model <- keras_model(
    inputs = inputs,
    outputs = classify
  )

  return(model)
}
