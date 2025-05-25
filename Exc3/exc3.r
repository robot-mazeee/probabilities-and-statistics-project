# Read the data and extract the desired columns
data = read.csv("Exc3/clima.csv")

# Extract data from the desired date
data$Data = as.POSIXct(data$Data)

data_jan2012 = data[
  format(data$Data, "%Y") == "2012" & format(data$Data, "%m") == "01",
]

# Create a column with date only
data_jan2012$Dia = as.Date(data_jan2012$Data)

# Calculate median
daily_median = aggregate(Velocidade_vento ~ Dia, data = data_jan2012, FUN = median, na.rm = TRUE)

daily_median$Dia = as.POSIXct(daily_median$Dia)

# Create graph
library(ggplot2)

graph = ggplot() +
  # Hourly wind speed
  geom_line(
    data = data_jan2012, 
    aes(x = Data, y = Velocidade_vento, color = "Velocidade do Vento Horária"),
    alpha = 0.6
  ) +
  # Daily median wind speed
  geom_line(
    data = daily_median, 
    aes(x = Dia, y = Velocidade_vento, color = "Mediana Diária")
  ) +
  # Manually set the colors
  scale_color_manual(
    values = c(
      "Velocidade do Vento Horária" = "blue", 
      "Mediana Diária" = "red"
    )
  ) +
  # Set labels and title
  labs(
    title = "Variação Horária da Velocidade do Vento - Janeiro de 2012",
    x = "Data e Hora",
    y = "Velocidade do Vento (m/s)",
    color = "Legenda"
  )

# Save the graph
ggsave("plot.png", graph, width = 10, height = 5)
