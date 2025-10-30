library(tidyverse)

root_dir <- "D:/BirdNET analysis BTE"

txt_files <- list.files(
  path = root_dir,
  pattern = "\\.txt$",
  full.names = TRUE,
  recursive = TRUE
)

detections <- data.frame(
  File = character(),    # full file path
  Folder = character(),  # parent folder name
  Species = character(),
  Time = character(),
  stringsAsFactors = FALSE
)

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
# Summarize detections by Species and Area where area 1 is folder BTE 01, area 2 is folder BTE 02, etc.
summary_df <- detections %>%
  mutate(Area = case_when(
    Folder == "BTE 01" ~ "Baturite 01",
    Folder == "BTE 02" ~ "Baturite 02",
    Folder == "BTE 03" ~ "Baturite 03",
    Folder == "BTE 04" ~ "Baturite 04",
    Folder == "BTE 05" ~ "Baturite 05",
    Folder == "BTE 06" ~ "Baturite 06",
    Folder == "BTE 07" ~ "Baturite 07",
    Folder == "BTE 08" ~ "Baturite 08",)) %>%
  group_by(Species, Area) %>%
  summarise(Detections = n(), .groups = 'drop')
View(summary_df)

 


# Example data
df <- tibble(
  Species = c("Ceara Gnateater", "Band-tailed Manakin", "Variable Antshrike",
              "Amazonian Motmot", "Bearded Bellbird"),
  Area = c("Baturite", "Aratanha"),
  Detections = c(10, 15, 5, 8, 12, 7)
)

# Plot
ggplot(df, aes(x = Area, y = Detections, fill = Species)) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(
    title = "Number of Detections per Species in Each Area",
    x = "Area",
    y = "Number of Detections",
    fill = "Species"
  ) +
  theme_minimal(base_size = 14)


# Plot grouped bar chart
ggplot(df, aes(x = Species, y = Detections, fill = Area)) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(
    title = "Number of Detections per Species by Area",
    x = "Species",
    y = "Number of Detections",
    fill = "Area"
  ) +
  theme_minimal(base_size = 14)