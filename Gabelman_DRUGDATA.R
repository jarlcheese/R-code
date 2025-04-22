library(tidyverse)
library(car)
library(dplyr)
library(ggplot2)

drug_data <- read_csv("drug_data.csv")

drug_data$drug <- factor(drug_data$drug)
levels(drug_data$drug)

drug_table <- drug_data %>%
  group_by(drug) %>%
  summarize(count = n())
drug_table

energy_rating_summary <- drug_data %>%
  group_by(drug) %>%
  summarize(
    average = mean(energy_rating, na.rm = TRUE),
    SE = sd(energy_rating, na.rm = TRUE) / sqrt(sum(!is.na(energy_rating)))
  )

energy_rating_aov <- aov(energy_rating ~ drug, drug_data)
summary(energy_rating_aov) # annova data
TukeyHSD(energy_rating_aov)
energy_rating_summary$tukeylabels <- c("a", "a", "b")
energy_rating_summary

rawplot <- ggplot(data = energy_rating_summary, aes(x = drug, y = average)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = average - SE, ymax = average + SE), width = 0.1)

combo <- rawplot +
  geom_jitter(data = drug_data, aes(x = drug, y = energy_rating),
              alpha = 0.3, size = 3, width = 0.1)

combo <- combo + 
  ylab("Energy Rating") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title=element_text(size=16, face="bold"),
        axis.text=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16, face="bold"))

energy_rating_resid <- residuals(energy_rating_aov)
shapiro.test(energy_rating_resid)

# testing for homogeneity of variance
leveneTest(energy_rating ~ drug, drug_data)

qqPlot(drug_data$energy_rating)
plot(energy_rating_aov, 1) 

print(rawplot)
print(combo)
