library(ggplot2)
library(dplyr)
library(maps)
library(cowplot)

setwd('SHAPE_FILE PATH')


data <- world.cities %>% filter(country.etc == "India")
aot <- read.csv('2021-ECL.csv')
aec <- right_join(data,aot, by=c("name"="name"),suffix = c(".x",".y"), keep = FALSE, na_matches="na")

myData <- aec#[1:22,]
myData$ECL_Dollar_M <- myData$ECL_Dollar/1000000
#myData$ECL_Dollar_per_Capita <- myData$ECL_Dollar/myData$pop

# https://github.com/AnujTiwari/India-State-and-Country-Shapefile-Updated-Jan-2020?tab=readme-ov-file#readme
# https://github.com/AnujTiwari/India-State-and-Country-Shapefile-Updated-Jan-2020/blob/master/India_Country_Boundary.shp
# https://www.diva-gis.org/gdata
IND <- tempfile()
unzip(zipfile='in_countries.zip', exdir=IND)
ind <- sf::read_sf(IND)

pl_ECL <- myData %>%
  arrange(ECL_Dollar_M) %>%
  mutate(name = factor(name, unique(name))) %>%
  ggplot() +
  geom_sf(data = ind,, fill = "grey", alpha = 0.3) +
  geom_point(aes(x = long, y = lat, fill = ECL_Dollar_M), size = 3.5, shape = 21, color = 'black', alpha = 0.7) +
  #scale_size_continuous(range = c(1, 12), name = "ECL in million USD") +
  #scale_alpha_continuous(name = "ECL in million USD") + #, trans = "log",
  scale_color_viridis_c(option = "viridis", name = "ECL_Dollar_M") +#, trans = "identity") +
  xlim(60, 100) +
  ylim(0,40) +
  theme_void() +
  ggtitle("Cities by ECL for 2021") +
  #ylim(20, 30) +
  theme(legend.position = "right",
   text = element_text(color = "#22211d"),
   plot.margin = margin(r = 0.25, l = 0.25, unit = "cm"),
   plot.background = element_rect(fill = "#f5f5f2", color = NA),
   panel.background = element_rect(fill = "#f5f5f2", color = NA),
   plot.title = element_text(size = 14, hjust = 0.5, color = "#4e4d47"),
   legend.title = element_text(size = 11),
   legend.text = element_text(size = 11))
   
pl_AOT40 <- myData %>%
  arrange(AOT40) %>%
  mutate(name = factor(name, unique(name))) %>%
  ggplot() +
  geom_sf(data = ind,, fill = "grey", alpha = 0.3) +
  geom_point(aes(x = long, y = lat, fill = AOT40), size = 3.5, shape = 21, color = 'black', alpha = 0.7) +
  #scale_size_continuous(range = c(1, 12), name = "ECL in million USD") +
  #scale_alpha_continuous(name = "ECL in million USD") + #, trans = "log",
  scale_color_viridis_c(option = "viridis", name = "AOT40 (ppb)") +#, trans = "identity") +
  xlim(60, 100) +
  ylim(0,40) +
  theme_void() +
  ggtitle("Cities by AOT40 for 2021") +
  #ylim(20, 30) +
  theme(legend.position = "right",
   text = element_text(color = "#22211d"),
   plot.margin = margin(r = 0.25, l = 0.25, unit = "cm"),
   plot.background = element_rect(fill = "#f5f5f2", color = NA),
   panel.background = element_rect(fill = "#f5f5f2", color = NA),
   plot.title = element_text(size = 14, hjust = 0.5, color = "#4e4d47"),
   legend.title = element_text(size = 11),
   legend.text = element_text(size = 11))

myPlot <- plot_grid(pl_ECL, pl_AOT40, labels = "AUTO")
save_plot("plot.pdf", myPlot, ncol = 2)
