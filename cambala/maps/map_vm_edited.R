library(ggmap)
library(mapproj)
library(maps)

library(rgeos) #этот пакет содержит какую-то хрень, которая позволяет обойти проблему пр чтении фалов средсвами maptools. 
#Att! Этот пакет должен быть загружен до maptools

library(mapdata)
library(maptools) # Rgshhs
library(PBSmapping)
library(gridExtra)
library(grid)


# install.packages("gpclib")

# install.packages("rgeos")

# install.packages("gpclib", type="source")


# library(rgdal)
theme_set(theme_bw() + theme(legend.key = element_blank()))
update_geom_defaults("point", list(shape = 19))


# Kand_x <- c(31.75, 35.5)
# Kand_y <- c(66, 67.25)
# Small_x <- c(32.3, 32.75)
# Small_y <- c(66.9, 67.1)



##############----- map in ggplot -----


# boundaries
Full_x <- c(-10, 50)
Full_y <- c(40, 72)


gshhs.l.b <- "E:/Lab/Data science/2020-2021/github/data_science2/cambala/maps/gshhs_l.b"


Kl <- getRgshhsMap(fn = gshhs.l.b, xlim = Full_x, ylim = Full_y)


dfKl <- fortify(Kl)

gg_full <- ggplot(dfKl, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") + coord_map(xlim = Full_x, ylim = Full_y) + theme_bw() +  theme(axis.title.x =element_blank(),  axis.title.y= element_blank(), plot.background = element_rect(fill = "white"), panel.grid = element_blank()) + theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + theme(axis.ticks = element_blank()) 

gg_full

point_cor <- read.table("E:/Lab/Data science/2020-2021/github/data_science2/cambala/data/point_coordinates.csv", sep = ",", header = TRUE)

point_cor$Lat <- point_cor$Lat + point_cor$Lat_m/60

point_cor$Long <- point_cor$Long + point_cor$Long_m/60

cambala <- read.table("E:/Lab/Data science/2020-2021/github/data_science2/cambala/data/point_proportion.csv", sep = ",", header = TRUE)

gg_full + geom_point(data = point_cor, aes(x = Long, y = Lat, group = 1, size = cambala$Left_prop, color = cambala$Left_prop)) + scale_color_gradient(high = 'darkblue', low = 'white')

















