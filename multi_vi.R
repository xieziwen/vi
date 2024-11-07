rm(list = ls())

library(tidyverse)
library(rio)
library(raster)
library(EBImage)


# Image Import Path
floder_path <- "Rdata/"

# Chose Multispectral Sensor 
sensor_type <- "RedEdge" #"DJI" or "PARROT" or "RedEdge" or "Sentinel-2"

# Defind DN Value or Reflectance
raster_type <- "DN" #"RE" or "DN"

# Result Save Path
output_path <- "Rresult/"





res <- data.frame()

# Defind Each Bands(num or NA)
if (sensor_type == "RedEdge") {
  GRE <- 1
  NIR <- 4
  BLU <- NA
  RED <- 2
  REG <- 3
} else if (sensor_type == "DJI"){
  GRE <- 1
  NIR <- 4
  BLU <- NA
  RED <- 2
  REG <- 3
} else if (sensor_type == "RedEdge"){
  GRE <- 1
  NIR <- 4
  BLU <- NA
  RED <- 2
  REG <- 3
} else {
  GRE <- 3
  NIR <- 8
  BLU <- 2
  RED <- 4
  REG <- 6 #740nm
}


file_path <- list.files(floder_path, full.names = T)
file_name <- list.files(floder_path)
dir_name <- floder_path

out_floder <- paste0(output_path, substr(floder_path, 7, nchar(floder_path)))
dir.create(out_floder)


res <- data.frame()

for (i in 1:length(file_path)) {
  # i <- 2
  img_file <- file_path[i]
  name_file <- file_name[i]
  img <- brick(img_file)
  plot(img)
  
  pixels <- dim(img)[1]*dim(img)[2]
  
  img_name <- substr(name_file, 1, nchar(name_file)-4)
  
  img_out <- paste0(out_floder, img_name)
  dir.create(img_out)
  
  # Extract Each Bands
  if (raster_type == "DN") {
    rv = 32768
  } else {
    rv = 1
  }
  
  calculate_band <- function(index) {
    if (!is.na(index)) {
      return(img[[index]] / rv)
    } else {
      return(NA)
    }
  }
  
  gre <- calculate_band(GRE)
  nir <- calculate_band(NIR)
  blu <- calculate_band(BLU)
  red <- calculate_band(RED)
  reg <- calculate_band(REG)
  
  
  vi_fun <- import("vi_fun_multi.csv")
  vi_list <- vi_fun[, 1]
  
  df <- data.frame(plot = img_name)
  for (i in 1:length(vi_list)) {
    vi_name <- as.character(vi_list[i])
    fun <- filter(vi_fun, index == vi_name)
    # Caculate vegetation index image
    vi = eval(parse(text = fun[, 2]))
    # Output vegetation index image
    # vi_img <- as.Image(vi)
    plot(vi)
    writeRaster(vi, paste0(img_out, "/", vi_name,".tif"), overwrite = TRUE)
    # Caculate vegetation indeies
    vi_array <- sort(as.numeric(vi@data@values))
    vi_v <- mean(vi_array[round(pixels*0.1):round(pixels*0.9)])
    vi_df <- data.frame(vi_v)
    colnames(vi_df) <- vi_name
    
    df <- cbind(df, vi_df)
  }
  
  res <- rbind(res, df)
}





write.csv(res, paste0(out_floder, "vi.csv"), row.names = F)

