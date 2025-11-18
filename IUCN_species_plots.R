# Load required packages
library(dplyr)
library(ggplot2)
library(showtext)

# Load the dataset
red.list.birds <- read_csv("~/Downloads/redlist_species_data_4b4283ea-f08a-443a-93e8-f044d136b598/assessments.csv")

# ---- 1. Count number of species by population trend in the Neotropical realm ----
trend_neotropical <- red.list.birds %>%
  filter(grepl("Neotropical", realm, ignore.case = TRUE)) %>%
  filter(populationTrend %in% c("Decreasing", "Stable", "Increasing")) %>%
  group_by(populationTrend) %>%
  summarise(species_count = n()) %>%
  ungroup() %>%
  mutate(
    total = sum(species_count),
    perc = (species_count / total) * 100
  )

# Add and enable the font
font_add_google("Work Sans", "work_sans")
showtext_auto()

# Plot
ggplot(trend_neotropical, aes(x = populationTrend, y = species_count, fill = populationTrend)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = paste0(round(perc, 1), "%")),
            vjust = 1.5, color = "white", size = 5, fontface = "bold", family = "work_sans") +
  scale_fill_manual(values = c("Decreasing" = "brown2", "Stable" = "burlywood3", "Increasing" = "darkolivegreen3")) +
  labs(
    title = "Population Trends of Neotropical Bird Species",
    x = "Population Trend",
    y = "Number of Species"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    panel.grid = element_blank(),
    text = element_text(family = "work_sans", color = "white"),
    plot.title = element_text(face = "bold", family = "work_sans"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )



-------------------------------------------------------------------------------
  
  
  # ---- 1. Count number of species by population trend worldwide ----
trend_worldwide <- red.list.birds %>%
  filter(populationTrend %in% c("Decreasing", "Stable", "Increasing")) %>%
  group_by(populationTrend) %>%
  summarise(species_count = n()) %>%
  ungroup() %>%
  mutate(
    total = sum(species_count),
    perc = (species_count / total) * 100
  )
# Plot
ggplot(trend_worldwide, aes(x = populationTrend, y = species_count, fill = populationTrend)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = paste0(round(perc, 1), "%")),
            vjust = 1.5, color = "white", size = 5, fontface = "bold", family = "work_sans") +
  scale_fill_manual(values = c("Decreasing" = "brown2", "Stable" = "burlywood3", "Increasing" = "darkolivegreen3")) +
  labs(
    title = "Population Trends of Worldwide Bird Species",
    x = "Population Trend",
    y = "Number of Species"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    panel.grid = element_blank(),
    text = element_text(family = "work_sans", color = "white"),
    plot.title = element_text(face = "bold", family = "work_sans"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )

# Pie chart
ggplot(trend_worldwide, aes(x = "", y = species_count, fill = populationTrend)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +  # Converts bar to pie chart
  geom_text(aes(label = paste0(round(perc, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, family = "work_sans", fontface = "bold") +
  scale_fill_manual(values = c("Decreasing" = "brown2", 
                               "Stable" = "burlywood3", 
                               "Increasing" = "darkolivegreen3")) +
  labs(
    title = "Population Trends of Worldwide Bird Species",
    fill = "Population Trend"
  ) +
  theme_void(base_size = 10) +  # Removes axes and grid
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    text = element_text(family = "work_sans", color = "white"),
    plot.title = element_text(face = "bold", family = "work_sans", hjust = 0.5),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )



--------------------------------------------------------------------------------
  
  # ---- 1. Create the dataset ----
data <- data.frame(
  Country = c("Colombia", "Brazil", "Peru", "Ecuador", "Bolivia"),
  Total_species = c(1962, 1872, 1855, 1621, 1416),
  Threatened_species = c(120, 166, 108, 99, 54)
)