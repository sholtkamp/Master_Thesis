# #target  = raster::brick("F:/Master_Thesis/data/target.tif")
# 
# b = subset(target, 1)
# b = raster::subset(target, 1)
# g = raster::subset(target, 2)
# r = raster::subset(target, 3)
# nir = raster::subset(target, 4)
# re = raster::subset(target, 5)
# 
# blue =  raster::as.array(b)
# green =  raster::as.array(g)
# red =  raster::as.array(r)
# nir =  raster::as.array(nir)
# re =  raster::as.array(re)
# 
# arr_blue = raster::as.array(blue)
# arr_green = raster::as.array(green)
# arr_red = raster::as.array(red)
# arr_nir = raster::as.array(nir)
# arr_re = raster::as.array(re)
# 
# drop_blue = drop(arr_blue)
# drop_green = drop(arr_green)
# drop_red = drop(arr_red)
# drop_nir = drop(arr_nir)
# drop_re = drop(arr_re)
# 
# target_array = array(dim = c(1152, 1116, 5))
# 
# target_array[,,1] <- drop_blue
# target_array[,,2] <- drop_green
# target_array[,,3] <- drop_red
# target_array[,,4] <- drop_nir
# target_array[,,5] <- drop_re
# 
# target = target_array
# 
# target_array = target_array/2
