pacman::p_load(reticulate)

# Function to convert RDS or RData file to PKL
convert_to_pkl <- function(file_path) {
  file_ext <- tools::file_ext(file_path)
  output_file <- sub("\\.(rds|RData)$", ".pkl", file_path)
  
  if (file_ext == "rds") {
    data <- readRDS(file_path)
  } else if (file_ext == "RData") {
    load(file_path)
    # Assuming the RData file contains only one object. 
    # Modify this line if there are multiple objects or if the object's name is known.
    data <- mget(ls())[1] 
  } else {
    stop("Unsupported file format")
  }
  
  py_save_object(data, output_file)
  cat("Converted", file_path, "to", output_file, "\n")
}

# List of RDS and RData files to convert
files_to_convert <- c(
  "./data/intermediate/base_avis_ccne.rds"
)

# Convert files
for (file_path in files_to_convert) {
  convert_to_pkl(file_path)
}
