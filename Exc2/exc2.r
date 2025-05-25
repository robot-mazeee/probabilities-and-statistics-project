library(readxl)

# Read and filter the data
data = read_excel("Exc2/wine_prod_EU.xlsx")
data = subset(data, Category != '-' & `Product Group` != 'Non-Vinified')

# Filter year 2000
data2000 = subset(data, Year == 2000)

library(ggplot2)

data2000$`Member State`[
  data2000$`Member State` != 'France' & 
  data2000$`Member State` != 'Italy' &
  data2000$`Member State` != 'Spain'] <- 'Others'

ggplot(data2000, aes(x = Category, y = Production, fill = `Member State`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Wine Production by Category in 2000",
    x = "Category",
    y = "Production"
  )

# Get Production by country in the year 2000
# france = subset(data2000, `Member State` == 'France')$Production
# italy = subset(data2000, `Member State` == 'Italy')$Production
# spain = subset(data2000, `Member State` == 'Spain')$Production
# germany = subset(data2000, `Member State` == 'Germany')$Production
# portugal = subset(data2000, `Member State` == 'Portugal')$Production
# others = subset(data2000, !`Member State` %in% c(
#     'France', 'Italy', 'Spain', 'Germany', 'Portugal'
# ))$Production

# # Create a summary data frame
# prod_data <- data.frame(
#   Country = c("France", "Italy", "Spain", "Germany", "Portugal", "Others"),
#   Production = c(sum(france), sum(italy), sum(spain), sum(germany), sum(portugal), sum(others))
# )

# Plot bar graph
# library(ggplot2)

# barplot = ggplot(prod_data, aes(x = Category, y = Production, fill = Country)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Wine Production by Country (Year 2000)",
#        y = "Production",
#        x = "Category")

ggsave("plot.png", width = 10, height = 8)
