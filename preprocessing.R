#Housekeeping
rm(list = ls())
graphics.off()


#load libraries
library(tidyverse)
library(ggplot2)
library(skimr)

path <- "Data"

#list of files
file_list <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

process_files <- function(file_path) {
  
  #read file
  df <- read.csv(file_path)
  
  return(df)
}

#process and combine
data <- lapply(file_list, process_files) %>% 
  bind_rows()

data <- data %>%
  filter(trial_type == "html-slider-response") %>%
  filter(grepl("images", image_path)) %>%
  mutate(
    scale = ifelse(grepl("Blobby", slider_labels, ignore.case = TRUE), "BlobbyStripey",
                   ifelse(grepl("Rounded", slider_labels, ignore.case = TRUE), "RoundedRectangular",
                          ifelse(grepl("Fine", slider_labels, ignore.case = TRUE), "FineCoarse",
                                 ifelse(grepl("Rough", slider_labels, ignore.case = TRUE), "RoughSmooth",
                                        ifelse(grepl("Random", slider_labels, ignore.case = TRUE), "RandomNon-Random",
                                               ifelse(grepl("Hard", slider_labels, ignore.case = TRUE), "HardSoft",
                                                      NA_character_))))))) %>% 
  select(subject_id, response, image_path, scale)
  
write.csv(data, "cleaned_texture_dimension_ratings.csv", row.names = FALSE)
