library(tidyverse)
library(car)
library(dplyr)

tig_data <- read_csv("Tigriopus_morphology_data.csv")

tig_data$population <- factor(tig_data$population)
levels(tig_data$population)

counts_table <- tig_data %>%
  group_by(population) %>%
  summarize(count = n())
counts_table

HL_summary <- tig_data %>%
  group_by(population) %>%
  summarize(
    average = mean(HL, na.rm =TRUE),
    SE = sd(HL, na.rm =TRUE)/sqrt(length(!is.na(HL)))
    )
HL_summary

HL_aov <- aov(HL ~ population, tig_data)

summary(HL_aov) # annova data

TukeyHSD(HL_aov)

HL_summary$tukeylabels <- c("a", "b", "c", "c")
HL_summary

# Plotting the mean and standard error bars
HL_rawplot <- ggplot(data = HL_summary, mapping = aes(x = Population, y = Average)) +
  geom_point(aes(shape = population), size = 4) +
  geom_errorbar(data = HL_summary, 
                aes(x = population, ymin=average-SE, ymax=average+SE), 
                width=0.1)

# Adding tukey labels to combo graph
HL_rawplot <- HL_rawplot + 
  geom_text(data = HL_summary, aes(x=population, y=average+SE+.04, label=tukeylabels), size = 6)

# Plotting the raw data points
HL_combo <- HL_rawplot +
  geom_jitter(data = tig_data, aes(x = population, y = HL, shape=population),
              alpha=0.3, size = 3, width=0.1)

# Making the plot pretty
HL_combo <- HL_combo + 
  ylab("Head length (mm)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title=element_text(size=16, face="bold"),
        axis.text=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16, face="bold"))

HL_resid <- residuals(HL_aov)
shapiro.test(HL_resid)

# testing for homogeneity of variance
leveneTest(HL ~ population, tig_data)

HL_rawplot # raw data plot
HL_combo # plot with tukey labels and pretty
qqPlot(tig_data$HL) # deviations from normality
plot(HL_aov, 1) # annova plot


