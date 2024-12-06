#Housekeeping
rm(list = ls())
graphics.off()


#load libraries
library(tidyverse)
library(ggplot2)
library(skimr)
library(reshape2)

#load dimension scale rating data

scales <- read.csv("cleaned_texture_dimension_ratings.csv")

#load PCA coordinates
pca <- read.csv("3dimension_pca.csv")

#update image paths
pca$images <- str_replace(pca$images, "materials/imgs/", "images/")

#match pca columns in scales
scales <- scales %>% 
  mutate(images = image_path)

#add rescaled column
scales <- scales %>%
  mutate(RescaledResponse = ((response + 200) * 1)/400)

scales <- scales %>%
  group_by(scale, images) %>%
  mutate(avg_rating = mean(RescaledResponse, na.rm = TRUE)) %>%
  ungroup() 

#combine data 
data <- left_join(scales, pca, by = "images")

# Calculate the correlation between PCA components and avg_rating
correlations <- data %>%
  group_by(scale) %>%
  summarise(
    cor_PC1 = cor(PC1, avg_rating, use = "complete.obs"),
    cor_PC2 = cor(PC2, avg_rating, use = "complete.obs"),
    cor_PC3 = cor(PC3, avg_rating, use = "complete.obs")
  )



# Pivot the data so that each scale has a column of avg_rating values per image
reshaped_data <- data %>%
  pivot_wider(names_from = scale, values_from = avg_rating)

# Now reshaped_data has a column for each scale, with the avg_rating for each image
# If multiple scales for each image, this step will align them properly

# Remove unnecessary columns (subject_id, response, image_path, images, etc.)
data_for_corr <- reshaped_data %>%
  select(-subject_id, -response, -image_path, -images, -RescaledResponse, -PC1, -PC2, -PC3) %>%
  drop_na()  # Remove rows with any NA values (if any scale is missing for an image)

#calculate the correlation matrix for the scales
cor_matrix <- cor(data_for_corr, use = "complete.obs")  

#Print
print(cor_matrix)


#Plots
#scales with each other
# Set column and row names for the correlation matrix
colnames(cor_matrix) <- rownames(cor_matrix) <- c("BlobbyStripey", "FineCoarse", "HardSoft", "RoughSmooth", "RandomNon-Random", "RoundedRectangular")

# Reshape the matrix into a long format for ggplot
cor_matrix_long <- melt(cor_matrix)

# Plot the heatmap
ggplot(cor_matrix_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "Scale", y = "Scale")

#Heatmap
#scales with PCAs
# Reshape the data for heatmap
correlations_melted <- melt(correlations, id.vars = "scale", variable.name = "PCA_Component", value.name = "Correlation")

# Plot the heatmap
ggplot(correlations_melted, aes(x = PCA_Component, y = scale, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, name = "Correlation") +
  theme_minimal() +
  labs(title = "Heatmap of PCA Component Correlations by Scale",
       x = "PCA Component", 
       y = "Scale") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# #Bar
# # Convert correlations to long format for easier plotting
# correlations_long <- correlations %>%
#   pivot_longer(cols = starts_with("cor_PC"), 
#                names_to = "PCA_Component", 
#                values_to = "Correlation")
# 
# # Plot the correlations
# ggplot(correlations_long, aes(x = scale, y = Correlation, fill = PCA_Component)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(~ PCA_Component, scales = "free_y") +  # Create separate plots for each PCA component
#   theme_minimal() +
#   labs(title = "Correlation between PCA Components and Scale",
#        x = "Scale", 
#        y = "Correlation",
#        fill = "PCA Component") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate correlations and p-values for PCA components and avg_rating
correlations2 <- data %>%
  group_by(scale) %>%
  summarise(
    cor_PC1 = cor(PC1, avg_rating, use = "complete.obs"),
    cor_PC2 = cor(PC2, avg_rating, use = "complete.obs"),
    cor_PC3 = cor(PC3, avg_rating, use = "complete.obs"),
    p_PC1 = cor.test(PC1, avg_rating)$p.value,
    p_PC2 = cor.test(PC2, avg_rating)$p.value,
    p_PC3 = cor.test(PC3, avg_rating)$p.value
  )

# Pivot data for correlation matrix
reshaped_data2 <- data %>%
  pivot_wider(names_from = scale, values_from = avg_rating)

# Remove unnecessary columns
data_for_corr2 <- reshaped_data2 %>%
  select(-subject_id, -response, -image_path, -images, -RescaledResponse, -PC1, -PC2, -PC3) %>%
  drop_na()  # Remove rows with any NA values
correlations2
