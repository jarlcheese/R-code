library(ggplot2)

data <- data.frame(
  TrailType = c("Informally Trailed,\nLow Hiker Visitation", 
                "Formally Trailed,\nLow Hiker Visitation", 
                "Formally Trailed,\nHigh Hiker Visitation"),
  NumBirds = c(38.167, 29.57, 18.87),
  StdError = c(1.7, 1.7, 0.74)
)

ggplot(data, aes(x = reorder(TrailType, -NumBirds), y = NumBirds, fill = TrailType)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = NumBirds - StdError, ymax = NumBirds + StdError), width = 0.2) +
  scale_fill_manual(values = c("Informally Trailed,\nLow Hiker Visitation" = "forestgreen", 
                               "Formally Trailed,\nLow Hiker Visitation" = "orange", 
                               "Formally Trailed,\nHigh Hiker Visitation" = "red")) +
  labs(x= "", y = "# all birds") +
  theme_minimal() +
  theme(legend.position = "none")
