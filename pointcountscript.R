library(tidyverse)
library(ggplot2)
library(car)
library(dplyr)

bird_data$Species <- as.character(bird_data$Species)
bird_data <- bird_data %>%
  mutate(`Sampling Day` = as.Date(`Sampling Day`))

# read CSV file
bird_data <- read_csv("point_count_data_sheet.csv")  # Replace with your actual file path

bird_data <- bird_data %>%
  mutate(`Sampling Day` = as.Date(`Sampling Day`))

# counts per day
daily_counts <- bird_data %>%
  group_by(`Sampling Day`) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE))

daily_counts$`Sampling Day` <- as.Date(daily_counts$`Sampling Day`)  # ensure Date class

daily_counts <- daily_counts %>%
  arrange(`Sampling Day`)  # sort by date

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


