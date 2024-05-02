library(ggplot2)
library(dplyr)
library(maps)
library(tidyr)
#library(patchwork)
#library(sf)
#library(RColorBrewer)


mybreaks <- c(100, 500, 1000, 5000, 10000, 20000)

states<-as.data.frame(state.x77)
states$region <- tolower(rownames(states))

states_map <- map_data("state")
fact_join <- left_join(states_map, states, by = "region")
ggplot(fact_join, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Population), color = "white")+
  scale_fill_fermenter(palette = "YlOrRd", name = "ECL_Dollar_M", breaks = mybreaks)+
  theme_light()+
  theme(
  panel.background = element_rect(fill = NA),
  panel.ontop = TRUE
)


