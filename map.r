# making a map

library(usmap)
library(tidyverse)

plot_usmap(include = c("PA", "NY", "OH", "NE"))

install.packages("maps")
library(maps)
map("state", interior = FALSE)
map('state', region = c('illinois', 'indiana', 'iowa', 'ohio'), fill=TRUE, col="red", add=TRUE)    # map of four states
map.text("state", regions=c("illinois", "indiana", "iowa", "ohio"), labels=as.character(c("IL", "IN", "IA", "OH")), add=TRUE) #add label

all_states <- map_data("state")  
p <- ggplot()


unique(all_states$region)
ce2 <- filter(all_states, region %in% c("new york", "pennsylvania","delaware","florida","illinois","kentucky",
                                        "kansas","iowa","maryland","nebraska","ohio","vermont","texas","virginia",
                                        "north carolina","wisconsin"))

install.packages("ggthemes")
library(ggthemes)
new <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="gray45", fill= "grey27") +
  geom_polygon(data = ce2, aes(x=long, y=lat, group = group),color = "gray45", fill = 'lightblue')+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
       )

