library(tidyverse)
library(ggplot2)
library(car)
library(dplyr)

bird_data <- read_csv("point_count_data_sheet.csv")  # Replace with your actual file path

bird_data$Species <- as.character(bird_data$Species)

bird_data <- bird_data %>%
  mutate(`Sampling Day` = as.Date(`Sampling Day`))

bird_data <- bird_data %>%
  mutate(`Migratory Status` = as.factor(`Migratory Status`))

# counts per day
daily_counts <- bird_data %>%
  group_by(`Sampling Day`) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE))

count_plot <- ggplot(daily_counts, aes(x = `Sampling Day`, y = Total_Count, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue") +
  labs(
    title = "Total Bird Counts Over Time",
    x = "Sampling Day",
    y = "Total Count"
  ) +
  theme_minimal()

# species specific counts
species_daily_counts <- bird_data %>%
  group_by(`Sampling Day`, Species) %>%
  summarise(Species_Count = sum(Count, na.rm = TRUE), .groups = "drop")

species_daily_counts <- bird_data %>%
  group_by(`Sampling Day`, Species) %>%
  summarise(Species_Count = sum(Count, na.rm = TRUE), .groups = "drop")

species_plot <- ggplot(species_daily_counts, aes(x = `Sampling Day`, y = Species_Count, color = Species, group = Species)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Bird Counts by Species Over Time",
    x = "Sampling Day",
    y = "Count"
  ) +
  theme_minimal()

# Summarize the data
migration_data <- bird_data %>%
  group_by(`Sampling Day`, `Migratory Status`) %>%
  summarise(
    Total_Count = sum(Count, na.rm = TRUE),
    SE = sd(Count, na.rm = TRUE) / sqrt(n()),  # standard error
    .groups = "drop"
  )

# Make the plot
migration_plot <- ggplot(migration_data, aes(x = `Sampling Day`, y = Total_Count, color = `Migratory Status`, group = `Migratory Status`)) +
  geom_line(size = 1) +   # Connect the points with lines
  geom_point(size = 3) +  # Show points too
  geom_errorbar(aes(ymin = Total_Count - SE, ymax = Total_Count + SE), width = 0.1) +
  scale_color_manual(values = c(
    "Breeding" = "forestgreen", 
    "Stopover" = "orange"
  )) +
  labs(
    title = "Bird Counts by Migratory Status Over Time",
    x = "Sampling Day",
    y = "Total Bird Count",
    color = "Migratory Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




