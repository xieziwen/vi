rm(list = ls())

library(tidyverse)
library(rio)
library(EBImage)

# Import Image
floder_path <- list.files("Rdata/", full.names = T) 
floder_name <- list.files("Rdata/")
type <- "RB"
res <- data.frame()

output_path <- "Rresult/"

for (i in 1:length(floder_path)) {
  # i <- 1
  file_path <- list.files(floder_path[i], full.names = T)
  file_name <- list.files(floder_path[i])
  dir_name <- floder_name[i]
  
  out_floder <- paste0(output_path, substr(floder_path[i], 7, nchar(floder_path[i])), "/")
  dir.create(out_floder)
  
  res <- data.frame()
  
  for (i in 1:length(file_path)) {
    img_file <- file_path[i]
    name_file <- file_name[i]
    img <- readImage(img_file)
    # display(img)
    
    pixels <- dim(img)[1]*dim(img)[2]
    
    img_name <- substr(name_file, 1, nchar(name_file)-4)
    
    img_out <- paste0(out_floder, img_name)
    dir.create(img_out)
    
    # Extract RGB bands
    r <- img[, , 1]
    g <- img[, , 2]
    b <- img[, , 3]
    
    vi_fun <- import("vi_fun_rgb.csv")
    vi_list <- vi_fun[, 1]
    
    df <- data.frame(plot = img_name)
    for (i in 1:length(vi_list)) {
      vi_name <- as.character(vi_list[i])
      fun <- filter(vi_fun, index == vi_name)
      # Caculate vegetation index image
      vi = eval(parse(text = fun[, 2]))
      # Output vegetation index image
      vi_img <- as.Image(vi) %>% 
        writeImage(., paste0(img_out, "/", vi_name,".tif"))
      # Caculate vegetation indeies
      vi_array <- sort(as.numeric(vi@.Data))
      vi_v <- mean(vi_array[round(pixels*0.1):round(pixels*0.9)])
      vi_df <- data.frame(vi_v)
      colnames(vi_df) <- vi_name
      
      df <- cbind(df, vi_df)
    }

    res <- rbind(res, df)
  }
  write.csv(res, paste0(output_path, dir_name, "vi.csv"), row.names = F)
}

