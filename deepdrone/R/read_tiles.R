#' Read directories of input tiles generated by create_subsets() into arrays for usage as input data
#'
#' @param inputdir String. Path to folder containing tile files.
#' @param dimensions Integer. Vector of input tile dimensions. Example: c(128, 128, 6)
#' @param format String. File format for output. Format needs to be supported by raster library.
#' @param channels Integer. Number of channels of input tile, e.g. 3 for RGB or 1 for masks.
#'
#' @return Multidimensional array of tile data.
#'
#' @examples \dontrun{read_tiles("C:/User/Desktop/Raster_Tiles/", c(128, 128), 6, ".tif")}
read_tiles <- function(inputdir, dimensions, channels, format){
  
  if(channels == 1){
    # determine parameters of input data
    n_input <- length(list.files(inputdir, pattern = paste0(format, "$")))
    
    # determine dimensions of inpout data
    input_size_x <- dimensions[1]
    input_size_y <- dimensions[2]
    
    # build array to hold data
    data <- array(dim = c(n_input, input_size_x, input_size_y))
    
    # setting up progress reports
    percent_input <- (n_input / 100)
    print("Reading data")
    progress_count <- 0
    temp <- 0
    
    # populate data array
    for(i in 1:n_input){
      # calculate part of file name according to naming conventions of createSubsets()
      lead <- (nchar(n_input) - nchar(i))
      zeros <- paste(replicate(lead, "0"), collapse = "")
      
      # read tile data into brick
      raster_file <- raster::raster(paste0(inputdir, "img_", zeros, i, format))
      
      
      channel_matrix <- raster::as.matrix(raster_file)
      
      # paste matrix into data array
      data[i, , ] <- channel_matrix[ , ]
    }
    
    # print progress of execution
    temp <- floor(i / percent_input)
    if(temp %% 10 == 0 && temp > 0 && temp > progress_count){
      print(paste0("Progress: ", temp, "%"))
      progress_count <- progress_count + 10
    }
  }
  
  else {
    # determine parameters of input data
    n_input <- length(list.files(inputdir, pattern = paste0(format, "$")))
    
    # determine dimensions of inpout data
    input_size_x <- dimensions[1]
    input_size_y <- dimensions[2]
    
    # build array to hold data
    data <- array(dim = c(n_input, input_size_x, input_size_y, channels))
    
    # setting up progress reports
    percent_input <- (n_input / 100)
    print("Reading data")
    progress_count <- 0
    temp <- 0
    
    # populate data array
    for(i in 1:n_input){
      # calculate part of file name according to naming conventions of createSubsets()
      lead <- (nchar(n_input) - nchar(i))
      zeros <- paste(replicate(lead, "0"), collapse = "")
      
      # read tile data into brick
      raster_file <- raster::brick(paste0(inputdir, "img_", zeros, i, format))
      
      # loop over channels to paste tile data into complete data array
      for(j in 1:channels){
        channel_matrix <- raster::as.matrix(raster::subset(raster_file, j))
        
        # paste matrix into data array
        data[i, , , j] <- channel_matrix[ , ]
      }
      
      # print progress of execution
      temp <- floor(i / percent_input)
      if(temp %% 10 == 0 && temp > 0 && temp > progress_count){
        print(paste0("Progress: ", temp, "%"))
        progress_count <- progress_count + 10
      }
    }
  }
  
  return(data)
}
