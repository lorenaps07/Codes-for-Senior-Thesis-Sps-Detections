library(stringr)

# Define your root directory
<<<<<<< HEAD
root_dir <- "E:/BirdNET analysis BTE"
=======
root_dir <- "D:/BirdNET analysis BTE"
>>>>>>> b5ad65d511128b3aa3b22b645a0e7b5ddd13a1b9

# Define the species you are searching for
desired_species <- c(
  "Ceara Gnateater", "Gould's Toucanet", "Short-tailed Anttrush",
  "Spot-winged Wood-Quail", "Band-tailed Manakin", "Variable Antshrike",
  "Amazonian Motmot", "Bearded Bellbird"
)

# Get all .txt files recursively
txt_files <- list.files(path = root_dir, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)

# Initialize a named vector to count detections
species_counts <- setNames(rep(0, length(desired_species)), desired_species)

# Loop through each file
for (file in txt_files) {
  # Try to read the file safely
  file_content <- tryCatch(readLines(file, warn = FALSE), error = function(e) NULL)
  
  if (!is.null(file_content)) {
    # For each species, count occurrences
    for (sp in desired_species) {
      matches <- str_count(file_content, fixed(sp))
      species_counts[sp] <- species_counts[sp] + sum(matches, na.rm = TRUE)
    }
  }
}

# Print the total counts
cat("Total detections per species:\n")
print(species_counts)

# Save the results to a CSV file
output_file <- file.path(root_dir, "species_detections_summary.csv")
write.csv(as.data.frame(species_counts), file = output_file, row.names = TRUE)
cat(paste("Summary saved to", output_file, "\n"))
# End of script
