rename_subsets <- function(){

current_dir <- getwd()
setwd("E:/Master_Daten/testing/masks")

# get number of tiles generated previously 
n_tiles <- length(list.files(".", pattern = "*.tif"))

# build vector of old file names (with gaps)
old_files <- list.files(".", pattern = "*.tif", full.names = TRUE)

# initiate vector of new file names (no gaps)
new_files <- vector(length = n_tiles)

# fill vector with new file names
for (i in 1:n_tiles){
  lead <- (nchar(n_tiles) - nchar(i))
  zeros <- paste(replicate(lead, "0"), collapse = "")
  new_files[i] <- paste0("img_", zeros, i, ".tif")
}

# rename files
file.rename(from = old_files, to = new_files)

# reset working directory
setwd(current_dir)
}