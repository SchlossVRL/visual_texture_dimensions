#Housekeeping
rm(list = ls())
graphics.off()


#load libraries
library(tidyverse)
library(ggplot2)
library(skimr)

path <- "Data/Data1"

#list of files
file_list <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

process_files <- function(file_path) {
  
  #read file
  df <- read_csv(file_path) #skip first row
  
  return(df)
}

#process and combine
data <- lapply(file_list, process_files) %>% 
  bind_rows()
