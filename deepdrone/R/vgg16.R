vgg_data_1 <- imagenet_preprocess_input(data[[1]][,,,1:5], data_format = NULL, mode = "tf")
vgg_data_2 <- imagenet_preprocess_input(data[[2]], data_format = NULL, mode = "tf")
vgg_data_3 <- imagenet_preprocess_input(data[[3]][,,,1:5], data_format = NULL, mode = "tf")
vgg_data_4 <- imagenet_preprocess_input(data[[4]], data_format = NULL, mode = "tf")


t1 <- k_constant(vgg_data_1)[,,,1:3]
t2 <- k_constant(vgg_data_3)[,,,1:3]
tiles <- k_concatenate(list(t1, t2), 1)


m1 <- k_constant(to_categorical(vgg_data_2, num_classes = 2))
m2 <- k_constant(to_categorical(vgg_data_4, num_classes = 2))
masks <- k_concatenate(list(m1, m2), 1)

vgg_target <- imagenet_preprocess_input(data[[5]][,,,1:3], data_format = NULL, mode = "tf")

#-------------------------------


inputs <- layer_input(name = "input_1", shape = c(128, 128, 3))

build_unet_vgg16 <- function(input_shape = c(128, 128, 3), num_classes = 2){
  
  vgg16_model <- application_vgg16(weights = "imagenet", include_top = FALSE, input_shape = input_shape, input_tensor = inputs)
  
  #---Downsampling------------------------------------------------------------------------------

    down_1 <- vgg16_model$layers[[4]]$output
    down_2 <- vgg16_model$layers[[7]]$output
    down_3 <- vgg16_model$layers[[11]]$output
    down_4 <- vgg16_model$layers[[15]]$output
    #freeze_weights(c(down_1, down_2, down_3, down_4))
  
  #---Center-----------------------------------------------------------------------------------
  center <- down_4 %>%
    layer_dropout(rate = 0.5) %>%
    layer_conv_2d(filters = 1024, kernel_size = c(3, 3), name = "center_1", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 1024, kernel_size = c(3, 3), name = "center_2", padding = "same") %>%
    layer_activation("relu")
  
  #---Upsampling--------------------------------------------------------------------------------
  
  up4 <- center %>%
    {layer_concatenate(inputs = list(down_4, .), axis = 3)} %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), name = "up_8", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 512, kernel_size = c(3, 3), name = "up_7", padding = "same") %>%
    layer_activation("relu")
  
  up3 <- up4 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down_3, .), axis = 3)} %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), name = "up_6", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), name = "up_5", padding = "same") %>%
    layer_activation("relu") 
  
  up2 <- up3 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down_2, .), axis = 3)} %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), name = "up_4", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 128, kernel_size = c(3, 3), name = "up_3", padding = "same") %>%
    layer_activation("relu") 
  
  up1 <- up2 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    {layer_concatenate(inputs = list(down_1, .), axis = 3)} %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), name = "up_2", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), name = "up_1", padding = "same") %>%
    layer_activation("relu") 
  
  add_up <- up1 %>%
    layer_upsampling_2d(size = c(2, 2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), name = "up_2_1", padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), name = "up_1_1", padding = "same") %>%
    layer_activation("relu") 
  
  #---Classification/Output---------------------------------------------------------------------
  classify <- layer_conv_2d(add_up, filters = num_classes, kernel_size = c(1, 1), activation = "sigmoid")
  
  # Build specified model and assign it to variable
  model <- keras_model(
    inputs = inputs,
    outputs = classify
  )
  
  return(model)
}

test_model <- build_unet_vgg16()
#compile
test_model %>% compile(
  loss = seg_loss, #"binary_crossentropy",
  optimizer = optimizer_sgd(lr = 0.001),
  metrics = c(dice, IoU)
)



# fit cloned model to training data
full_history <- test_model %>%
  fit_generator(
    epochs = 100,
    steps_per_epoch = dim(data[[1]])[1] / 16,
    callbacks = early_stopping_call_soft,

    generator = flow_images_from_data(
      batch_size = 16,
      tiles,
      masks,
      image_datagen,
      subset = "training"
    ),

    validation_data = flow_images_from_data(
      batch_size = 16,
      tiles,
      masks,
      image_datagen,
      subset = "validation"
    )
  )


full_evaluation = evaluate(test_model, test_tile_tensor[[1]], test_mask_tensor[[1]])

temp <- test_model %>% predict(vgg_target)




# merge predictions tiles
predictions_merged <- merge_tiles(temp[ , , , 2], columns = 8, overlap = FALSE)

# use target area information to georeference predictions
prediction_raster <- rasterize_predictions(target_extent, target_crs_pre, target_crs_post, predictions_merged)

# plot results of prediction
plot(prediction_raster, 
     main = "Class Probabilities for Target Area", 
     xlab = "Longitude [°E]", 
     ylab = "Lattitude [°N]", 
     col = cols,
     breaks = c(1.0, 0.95, 0.90, 0.85, 0.8, 0.75, 0.7, 0.65, 0.60, 0.55, 0.50)
)
writeRaster(prediction_raster, file = paste0("C:/Users/Basti/Desktop/", project_name, "/", model_name, "_probs_raster.tif"), format="GTiff", overwrite = TRUE)

png(paste0("C:/Users/Basti/Desktop/", project_name, "/", model_name, "_probs.png"), width=525, height=550, pointsize = 20)
plot(prediction_raster, 
     main = "Class Probabilities of Target Area", 
     xlab = "Longitude [°E]", 
     ylab = "Lattitude [°N]", 
     
     col = cols, 
     breaks = c(1.0, 0.95, 0.90, 0.85, 0.8, 0.75, 0.7, 0.65, 0.60, 0.55, 0.50),
     
     par(bty="l", cex.axis= 0.75, cex.lab = 0.85)
     
)
dev.off()
