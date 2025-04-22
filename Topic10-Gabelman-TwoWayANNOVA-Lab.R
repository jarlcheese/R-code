library(car)
library(tidyverse)
library(ggplot2)
library(dplyr)

drug_data <- read.csv("drugsexdata.csv")

drug_data$sex <- as.factor(drug_data$sex)

drug_table <- drug_data %>%
  group_by(drug, sex) %>%
  summarize(count = n())
drug_table

drug_summary <- drug_data %>%
  group_by(drug, sex) %>%
  summarize(
    average = mean(energy, na.rm =TRUE),
    SE = sd(energy, na.rm = TRUE)/sqrt(length(!is.na(energy)))
  )
drug_summary

drug_rawplot <- ggplot(data = drug_summary, mapping = aes(x = drug, y = average)) +
  geom_point(size = 4) +
  geom_errorbar(data = drug_summary, 
                aes(x = drug, ymin=average-SE, ymax=average+SE), 
                width=0.1) +
  facet_wrap(~sex)

drug_combo <- drug_rawplot +
  geom_jitter(data = drug_data, aes(x = drug, y = energy),
              alpha=0.3, size = 3, width=0.1)

drug_finalgraph <- drug_combo + 
  ylab("Energy") +
  xlab("Drug") +
  theme(panel.grid.major = element_blank(),
        axis.title=element_text(size=16, face="bold"),
        axis.text=element_text(size=14),
        legend.text=element_text(size=14)) +
  theme_minimal()

drug_aov <- aov(energy ~ drug * sex, drug_data)

summary(drug_aov) # result gives no statistically significant relationship (no p-values < 0.05)

drug_resid <- residuals(drug_aov)
shapiro.test(drug_resid) # fail to reject the null hyopthesis or normality

leveneTest(energy ~ drug * sex, drug_data) # variances are not significantly different

# post-hoc comparisons not necessary, but here is code for it
TukeyHSD(drug_aov)

interaction_plot <- ggplot(drug_data, aes(x = drug, y = energy, color = sex, group = sex)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Interaction Plot: Drug x Sex",
       x = "Drug",
       y = "Mean Energy Level",
       color = "Sex") +
  theme_minimal()

drug_rawplot
drug_combo
drug_aov
interaction_plot
