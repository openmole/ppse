library(plotly)

#spinrates <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/spinrates.csv",
#                      stringsAsFactors = FALSE)


spinrates <- read.csv("/tmp/traffic-map.csv",
                      stringsAsFactors = FALSE)


p <- ggplot(spinrates, aes(x=speed, y=patience)) +
  geom_tile(aes(fill = density)) +
  #scale_fill_distiller(palette = "YlGnBu") 
  scale_fill_distiller(palette = "Spectral")
  #labs(title = "Density of patterns and missing on a fastball",
  #     y = "spin rate (rpm)")

ggplotly(p)

