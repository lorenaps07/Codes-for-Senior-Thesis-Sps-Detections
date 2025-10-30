# example-of-data-use


``` r
library(stringr)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(readr)
```

``` r
root_dir <- "D:/BirdNET analysis BTE"

desired_species <- c(
  "Ceara Gnateater", "Gould's Toucanet", "Short-tailed Anttrush",
  "Spot-winged Wood-Quail", "Band-tailed Manakin", "Variable Antshrike",
  "Amazonian Motmot", "Bearded Bellbird"
)
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
```

# For Loops

``` r
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
```

# Print Results

``` r
print(species_summary)
```

    # A tibble: 7 × 3
      Species                Total_Detections Files_with_Species
      <chr>                             <int>              <int>
    1 Band-tailed Manakin               11618               1021
    2 Variable Antshrike                 7421                992
    3 Amazonian Motmot                   5179                673
    4 Ceara Gnateater                    1060                295
    5 Spot-winged Wood-Quail               11                 10
    6 Bearded Bellbird                     10                  5
    7 Gould's Toucanet                      1                  1

``` r
print(file_summary)
```

    # A tibble: 2,657 × 3
       Folder              File                                      Species_Present
       <chr>               <chr>                                     <chr>          
     1 SwiftOne_2025-02-05 D:/BirdNET analysis BTE/BTE 01/Gravações… Amazonian Motm…
     2 SwiftOne_2025-02-05 D:/BirdNET analysis BTE/BTE 03/Gravações… Band-tailed Ma…
     3 SwiftOne_2025-02-05 D:/BirdNET analysis BTE/BTE 03/Gravações… Band-tailed Ma…
     4 SwiftOne_2025-02-05 D:/BirdNET analysis BTE/BTE 03/Gravações… Band-tailed Ma…
     5 SwiftOne_2025-02-05 D:/BirdNET analysis BTE/BTE 03/Gravações… Amazonian Motm…
     6 SwiftOne_2025-02-05 D:/BirdNET analysis BTE/BTE 03/Gravações… Amazonian Motm…
     7 SwiftOne_2025-02-05 D:/BirdNET analysis BTE/BTE 03/Gravações… Amazonian Motm…
     8 SwiftOne_2025-02-05 D:/BirdNET analysis BTE/BTE 05/Gravações… Amazonian Motm…
     9 SwiftOne_2025-02-05 D:/BirdNET analysis BTE/BTE 06/Gravações… Amazonian Motm…
    10 SwiftOne_2025-02-06 D:/BirdNET analysis BTE/BTE 01/Gravações… Spot-winged Wo…
    # ℹ 2,647 more rows
