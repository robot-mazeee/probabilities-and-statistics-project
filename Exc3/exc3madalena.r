library(ggplot2)

dados <- read.csv("Exc3/clima.csv")

dados$Data <- as.Date(substr(dados$Data, 1, 10))

dados_outubro <- subset(dados, substr(dados$Data, 1, 7) == "2010-10")

dados_outubro$Dia <- dados_outubro$Data

mediana_diaria <- aggregate(Pressão~Dia, data = dados_outubro, FUN = median, na.rm = TRUE)

ggplot() +
  geom_line(data = dados_outubro, aes(x = Data, y = Pressão, color = "Pressão"), alpha = 0.5, linewidth = 1) +
  geom_line(data = mediana_diaria, aes(x = Dia, y = Pressão, color = "Mediana diária")) +
  labs(
    title = "Variação Horária da Pressão em outubro de 2010",
    x = "Data",
    y = "Pressão",
    color = "Legenda")

ggsave("plot.png", width = 10, height = 5)