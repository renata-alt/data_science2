
library(tidyverse)
library(lubridate)
library(ggmap)
library(ggrepel)
library(gridExtra)
library(pander)

troops <- read.table('data/troops.txt', header = TRUE)
cities <- read.table('data/cities.txt', header = TRUE)
temps <- read.table('data/temps.txt', header = TRUE) %>%
  mutate(date = dmy(date))

ggplot() +
  geom_path(data = troops, aes(x = long, y = lat, group = group, color = direction, size = survivors), lineend = 'round') + theme_bw() + 
  scale_size(range = c(0.5, 14)) +
  guides(color = 'none', size = 'none') +
  scale_color_manual(values = c('#DFC13E', 'black')) +
  labs(x = NULL, y = NULL) +
  geom_point(data = cities, aes(x = long, y = lat, group = 1)) +
  geom_text_repel(data = cities, aes(x = long, y = lat, label = city))

march.1812.europe <- c(left = 10, bottom = 50, right = 41.04, top = 58)

march.1812.europe.map <- get_stamenmap(bbox = march.1812.europe, zoom = 8,
                                       maptype = "watercolor", where = "cache")

ggmap(march.1812.europe.map) +
  geom_path(data = troops, aes(x = long, y = lat, group = group, color = direction, size = survivors), lineend = 'round') + theme_bw() + 
  scale_size(range = c(0.5, 14)) +
  guides(color = 'none', size = 'none') +
  scale_color_manual(values = c('#DFC13E', 'black')) +
  labs(x = NULL, y = NULL) +
  geom_point(data = cities, aes(x = long, y = lat, group = 1)) +
  geom_text_repel(data = cities, aes(x = long, y = lat, label = city))



























