# Load packages
library(stringr)
library(dplyr)
library(readr)

#-------------------------------
# 1. Define inputs
#-------------------------------
root_dir <- "D:/BirdNET analysis BTE"

desired_species <- c(
  "Ceara Gnateater", "Gould's Toucanet", "Short-tailed Anttrush",
  "Spot-winged Wood-Quail", "Band-tailed Manakin", "Variable Antshrike",
  "Amazonian Motmot", "Bearded Bellbird"
)

#-------------------------------
# 2. Get all text files
#-------------------------------
txt_files <- list.files(
  path = root_dir,
  pattern = "\\.txt$",
  full.names = TRUE,
  recursive = TRUE
)

#-------------------------------
# 3. Initialize storage
#-------------------------------
detections <- data.frame(
  File = character(),    # full file path
  Folder = character(),  # parent folder name
  Species = character(),
  Time = character(),
  stringsAsFactors = FALSE
)

#-------------------------------
# 4. Loop through files
#-------------------------------
for (file in txt_files) {
  file_content <- tryCatch(readLines(file, warn = FALSE), error = function(e) NULL)
  
  if (!is.null(file_content)) {
    folder_name <- basename(dirname(file))  # immediate parent folder
    
    for (sp in desired_species) {
      # Find lines mentioning this species
      matching_lines <- file_content[str_detect(file_content, fixed(sp))]
      
      if (length(matching_lines) > 0) {
        # Extract times (HH:MM or HH:MM:SS)
        times <- str_extract(matching_lines, "\\b\\d{1,2}:\\d{2}(?::\\d{2})?\\b")
        times[is.na(times)] <- "Unknown"
        
        # Store detections
        temp <- data.frame(
          File = normalizePath(file, winslash = "/"),  # full path, forward slashes for CSV compatibility
          Folder = folder_name,
          Species = sp,
          Time = times,
          stringsAsFactors = FALSE
        )
        detections <- rbind(detections, temp)
      }
    }
  }
}

#-------------------------------
# 5. Summaries
#-------------------------------

# Species summary
species_summary <- detections %>%
  group_by(Species) %>%
  summarise(
    Total_Detections = n(),
    Files_with_Species = n_distinct(File)
  ) %>%
  arrange(desc(Total_Detections))

# File-level summary (with folder)
file_summary <- detections %>%
  group_by(Folder, File) %>%
  summarise(Species_Present = paste(unique(Species), collapse = "; "),
            .groups = "drop")

#-------------------------------
# 6. Write results to CSV
#-------------------------------
write_csv(detections, file.path(root_dir, "detections_detailed.csv"))
write_csv(species_summary, file.path(root_dir, "detections_by_species.csv"))
write_csv(file_summary, file.path(root_dir, "detections_by_file.csv"))

#-------------------------------
# 7. Console output
#-------------------------------
cat("\n✅ CSV files successfully created in:", root_dir, "\n")
cat(" - detections_detailed.csv\n - detections_by_species.csv\n - detections_by_file.csv\n\n")

cat("=== Species Summary ===\n")
print(species_summary)

cat("\n=== Example of Detailed Detections ===\n")
print(head(detections, 10))
