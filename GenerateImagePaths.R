# Load necessary library
library(stringr)

# Define the directory containing the images
image_directory <- "images" 

# Get a list of all image files (e.g., .png, .jpg, etc.)
image_files <- list.files(image_directory, pattern = "\\.(png|jpg|jpeg)$", full.names = FALSE)

# Create the JavaScript object as a string
js_object <- "var textures = {\n"

# Iterate over the image files and create a key-value pair for each
for (i in seq_along(image_files)) {
  # Remove any leading or trailing whitespace from file names
  image_name <- str_trim(image_files[i])
  
  # Format the entry in the desired form
  js_entry <- paste0('  "', i - 1, '": "images/', image_name, '"')
  
  # Add a comma if it's not the last entry
  if (i < length(image_files)) {
    js_entry <- paste0(js_entry, ",")
  }
  
  # Add the entry to the JavaScript object string
  js_object <- paste0(js_object, js_entry, "\n")
}

# Close the JavaScript object
js_object <- paste0(js_object, "};")

# Print the JavaScript object (or write to file)
cat(js_object)

# Optionally write the output to a file (e.g., maps.js)
write(js_object, file = "imagePaths.js")
