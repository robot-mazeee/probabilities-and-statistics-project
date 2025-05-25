# Read the data and extract the desired columns
data = read.csv("Exc1/winequality-white-q5.csv")

data$density = sqrt(data$density)

library(ggplot2)

# Plot the boxplot
boxplot = ggplot(data, aes(x = as.factor(quality), y = density)) +
  geom_boxplot(fill = "lightblue", color = "darkblue",         
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3,
    alpha = 0.1
  ) +
  labs(
    title = "Square Root of Density by Wine Quality",
    x = "Wine Quality",
    y = "Square Root of Density"
  ) 

ggsave("plot.png", width = 10, height = 7)