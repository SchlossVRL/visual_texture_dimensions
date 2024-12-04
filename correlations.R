#Housekeeping
rm(list = ls())
graphics.off()


#load libraries
library(tidyverse)
library(ggplot2)
library(skimr)

#load dimension scale rating data

scales <- read.csv("cleaned_texture_dimension_ratings.csv")

#load PCA coordinates