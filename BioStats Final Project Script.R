library(tidyverse)
library(car)
library(dplyr)
library(ggplot2)

bacterium_data <- read_csv("bacterium_data.csv")
bacterium_data$Compound <- factor(bacterium_data$Compound)

# summary stats
bacterium_table <- bacterium_data %>%
  group_by(Compound) %>%
  summarize(count = n())
bacterium_table

bacterium_summary <- bacterium_data %>%
  group_by(Compound) %>%
  summarize(
    average = mean(`Inhibition (mm)`, na.rm = TRUE),
    SE = sd(`Inhibition (mm)`, na.rm = TRUE) / sqrt(sum(!is.na(`Inhibition (mm)`)))
  )
bacterium_summary

# annova
bacterium_aov <- aov(`Inhibition (mm)`~ Compound, bacterium_data)
summary(bacterium_aov) 

# tukey
tukey <- TukeyHSD(bacterium_aov)
p_values <- tukey$Compound[, "p adj"]
names(p_values) <- rownames(tukey$Compound)
library(multcompView)
tukey_letters <- multcompLetters(p_values)
tukey_letters$Letters
bacterium_summary$tukeylabels <- tukey_letters$Letters[as.character(bacterium_summary$Compound)]

# graphs
bacterium_plot <- ggplot(bacterium_summary, aes(x = Compound, y = average)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = average - SE, ymax = average + SE), width = 0.2) +
  theme_minimal() +
  labs(
    title = "Average Inhibition by Compound",
    x = "Compound",
    y = "Average Inhibition (mm)"
  )

bacterium_plot_tukey <- bacterium_plot +
  geom_text(
    data = bacterium_summary,
    aes(x = Compound, y = average + SE + 1, label = tukeylabels),
    size = 6
  )


# graphs
bacterium_plot # data plot
bacterium_plot_tukey # data plot with tukey labels
qqPlot(bacterium_data$`Inhibition (mm)`) # deviations from normality
plot(bacterium_aov, 1) # annova plot

