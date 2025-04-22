library(tidyverse)
library(car)
library(ggplot2)

copepod_data <- read.csv("copepod_HL_pH.csv")

copepod_data$pH <- as.factor(copepod_data$pH)

copepod_table <- copepod_data %>%
  group_by(population, pH) %>%
  summarize(count = n())
copepod_table

copepod_summary <- copepod_data %>%
  group_by(population, pH) %>%
  summarize(
    average = mean(HL, na.rm =TRUE),
    SE = sd(HL, na.rm = TRUE)/sqrt(length(!is.na(HL)))
  )
copepod_summary

copepod_rawplot <- ggplot(data = copepod_summary, mapping = aes(x = population, y = average)) +
  geom_point(size = 4) +
  geom_errorbar(data = copepod_summary, 
                aes(x = population, ymin=average-SE, ymax=average+SE), 
                width=0.1) +
  facet_wrap(~pH)

copepod_combo <- copepod_rawplot +
  geom_jitter(data = copepod_data, aes(x = population, y = HL),
              alpha=0.3, size = 3, width=0.1)

copepod_finalgraph <- copepod_combo + 
  ylab("Head length (mm)") +
  xlab("Population") +
  theme(panel.grid.major = element_blank(),
        axis.title=element_text(size=16, face="bold"),
        axis.text=element_text(size=14),
        legend.text=element_text(size=14)) +
  theme_bw()

copepod_aov <- aov(HL ~ population * pH, copepod_data)

summary(copepod_aov)

copepod_resid <- residuals(copepod_aov)
shapiro.test(copepod_resid)

plot(copepod_aov)

leveneTest(HL ~ population * pH, copepod_data)

#post-hoc comparisons
TukeyHSD(copepod_aov)


