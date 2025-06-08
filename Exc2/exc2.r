library(readxl)
library(ggplot2)

data = read_excel("Exc2/wine_prod_EU.xlsx")
data = subset(data, Category != '-' & `Product Group` != 'Non-Vinified')
data2000 = subset(data, Year == 2000)

data2000$`Member State`[
  data2000$`Member State` != 'France' & 
  data2000$`Member State` != 'Italy' &
  data2000$`Member State` != 'Spain' &
  data2000$`Member State` != 'Germany' &
  data2000$`Member State` != 'Portugal'] <- 'Others'

ggplot(data2000, aes(x = Category, y = Production, fill = `Member State`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Wine Production by Category in 2000",
    x = "Category",
    y = "Production"
  )

ggsave("plot.png", width = 5, height = 3)