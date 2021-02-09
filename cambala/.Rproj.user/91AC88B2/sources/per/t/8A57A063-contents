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

# boundaries
Full_x <- c(3, 43)
Full_y <- c(55, 72)
Kand_x <- c(31.75, 35.5)
Kand_y <- c(66, 67.25)
Small_x <- c(32.3, 32.75)
Small_y <- c(66.9, 67.1)





#----- google map ----
map1 <- get_map(location = c(28, 66), zoom = 4)
p1 <- ggmap(map1) + xlim(5, 43) + ylim(55, 71)

map2 <- get_map(location = c(32.6, 67), zoom = 6)
p2 <- ggmap(map2) + xlim(31.75, 35.5) + ylim(66, 67.25)

map3 <- get_map(location = c(32.6, 67), zoom = 10)
p3 <- ggmap(map3) + xlim(32.4, 32.8) + ylim(66.9, 67.1)

p <- arrangeGrob(p1, p2, nrow = 1, widths = c(0.4, 0.6))
p

library(gtable)
library(grid)
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
gup <- gtable:::cbind_gtable(g1, g2, "first")
#Remove a column between the plots
gup <- gtable_add_cols(gup, unit(-1,"cm"), pos = ncol(g1))
g <- arrangeGrob(gup, g3, ncol = 1, heights = c(0.31, 0.69))

grid.newpage()
grid.draw(g)

# grid.newpage()
# v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
# v2<-viewport(width = 0.3, height = 0.3, x = 0.675, y = 0.81) #plot area for the inset map
# print(p1,vp=v1)
# print(p2,vp=v2)

# + geom_point(aes(x = long, y = lat, colour = "red"), data = coords, alpha = .5)

#----- traditional map -----

map('worldHires', xlim = Small_x, ylim = Small_y, col = "gray90", fill = TRUE)
map('worldHires', xlim = Full_x, ylim = Full_y, col = "gray70", fill = TRUE)


#----- GSHHG data -----
# http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html

#files
# gshhs.c.b <- "/media/data/ProjectsWork/stat-2014-10-23-Dima/aside/gshhg/gshhg-bin-2.3.3/gshhs_c.b"

gshhs.l.b <- "d:/Data_LMBE/Maps/Gshhs/gshhs_l.b"

gshhs.f.b <- "d:/Data_LMBE/Maps/Gshhs/gshhs_f.b"

wdb_rivers.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_rivers_f.b"
wdb_borders.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_borders_f.b"


# gshhs.i.b <- "/media/data/ProjectsWork/stat-2014-10-23-Dima/aside/gshhg/gshhg-bin-2.3.3/gshhs_i.b"
# gshhs.h.b <- "/media/data/ProjectsWork/stat-2014-10-23-Dima/aside/gshhg/gshhg-bin-2.3.3/gshhs_h.b"



# read spatial data
# Land_c <- Rgshhs(gshhs.c.b, xlim = Full_x, ylim = Full_y, level = 1)

Land_l <- Rgshhs(gshhs.l.b, xlim = Full_x, ylim = Full_y, level = 1)
# Land_i <- Rgshhs(gshhs.i.b, xlim = Full_x, ylim = Full_y, level = 1)
# Land_h <- Rgshhs(gshhs.h.b, xlim = Full_x, ylim = Full_y, level = 1)
Land_f <- Rgshhs(gshhs.f.b, xlim = Kand_x, ylim = Kand_y, level = 1)
# Rivers <- Rgshhs(wdb_rivers.f.b, xlim = Full_x, ylim = Full_y)
# Borders <- Rgshhs(wdb_borders.f.b, xlim = Full_x, ylim = Full_y)





# Grayscale

png(filename = "FIG1-MAP.png", width = 8.5, height = 8, units = "cm", pointsize = 12, res = 300, type = "cairo-png")
par(mar = c(2.5, 2.5, 1, 1), cex = 0.7)
# Small
plot.window(xlim = c(130, 140), ylim = c(40, 70))
plot(Land_f$SP, col = "grey90", bg = "white", xlim = c(32.3, 32.75), ylim = c(66.9, 67.1), axes = T)
points(x = 32.566, y = 67.011, pch = 19, cex = 1.2)
box()

par(fig = c(0.03, 0.48, 0.03, 0.455), new = TRUE)
plot(Land_f$SP, col = "grey80", bg = "white", xlim = Kand_x, ylim = Kand_y, xaxs = "i", yaxs = "i", lwd = 0.7)
polygon(x=c(32.24,32.24,32.82,32.82),
        y=c(66.89,67.11,67.11,66.89), col="transparent")
box()

par(fig = c(0.03, 0.48, 0.506, 0.97), new = TRUE)
plot(Land_l$SP, col = "grey80", bg = "white", xlim = Full_x, ylim = Full_y, xaxs = "i", yaxs = "i", lwd = 0.64)

polygon(x=c(31.8,31.8,35.4,35.4),
        y=c(66,67.26,67.26,66), col="transparent")
box()

par(fig = c(0, 1, 0, 1), new = TRUE)
lines(x = c(10.2, -2.7), y = c(69.75, 59.4))
lines(x = c(11.8, 15.2), y = c(69.75, 59.4))

dev.off()

# Coloured

png(filename = "FIG1-MAP-coloured.png", width = 8.5, height = 8, units = "cm", pointsize = 12, res = 300, type = "cairo-png")
par(mar = c(2.5, 2.5, 1, 1), cex = 0.7)
# Small
plot.window(xlim = c(130, 140), ylim = c(40, 70))
plot(Land_f$SP, col = "ivory", bg = "azure", xlim = c(32.3, 32.75), ylim = c(66.9, 67.1), axes = T)
points(x = 32.566, y = 67.011, pch = 21, cex = 1.2, col = "black", bg = "red")
box()

par(fig = c(0.03, 0.48, 0.03, 0.455), new = TRUE)
plot(Land_f$SP, col = "ivory2", bg = "azure1", xlim = Kand_x, ylim = Kand_y, xaxs = "i", yaxs = "i", lwd = 0.7)
polygon(x=c(32.24,32.24,32.82,32.82),
        y=c(66.89,67.11,67.11,66.89), col="transparent")
box()

par(fig = c(0.03, 0.48, 0.506, 0.97), new = TRUE)
plot(Land_l$SP, col = "ivory2", bg = "azure1", xlim = Full_x, ylim = Full_y, xaxs = "i", yaxs = "i", lwd = 0.64)

polygon(x=c(31.8,31.8,35.4,35.4),
        y=c(66,67.26,67.26,66), col="transparent")
box()

par(fig = c(0, 1, 0, 1), new = TRUE)
lines(x = c(10.2, -2.7), y = c(69.75, 59.4))
lines(x = c(11.8, 15.2), y = c(69.75, 59.4))

dev.off()


# lines(Rivers$SP, col="lightblue")
# lines(Borders$SP, col="darkred")


##############----- map in ggplot -----


# boundaries
Full_x <- c(3, 50)
Full_y <- c(55, 72)
Kand_x <- c(31.75, 35.5)
Kand_y <- c(66, 67.25)
Small_x <- c(32.3, 32.75)
Small_y <- c(66.9, 67.1)



gshhs.l.b <- "d:/Data_LMBE/Maps/Gshhs/gshhs_l.b"

gshhs.f.b <- "d:/Data_LMBE/Maps/Gshhs/gshhs_f.b"
wdb_rivers.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_rivers_f.b"
wdb_borders.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_borders_f.b"




Kl <- getRgshhsMap(fn = gshhs.l.b, xlim = Full_x, ylim = Full_y)

Kf <- getRgshhsMap(fn = gshhs.f.b, xlim = Small_x, ylim = Small_y)

dfKl <- fortify(Kl)
dfKf <- fortify(Kf)

gg_full <- ggplot(dfKl, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") + coord_map(xlim = Full_x, ylim = Full_y) + theme_bw() +  theme(axis.title.x =element_blank(),  axis.title.y= element_blank(), plot.background = element_rect(fill = "white"), panel.grid = element_blank()) + theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + theme(axis.ticks = element_blank()) 





gg_full


gg_small <- ggplot(dfKf, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") +
  coord_map(xlim = Small_x, ylim = Small_y)+theme_bw()+xlab("")+ylab("")+theme(axis.text.y =element_text(angle = 90, hjust=0.5))



library(gridExtra)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.3, height = 0.3, x = 0.675, y = 0.81) #plot area for the inset map
print(gg_small,vp=v1)
print(gg_full,vp=v2)



###############################


# polys <- importGSHHS("/media/data/ProjectsWork/stat-2014-10-23-Dima/aside/gshhg/gshhg-bin-2.3.3/gshhs_f.b" , xlim = Full_x , ylim = Full_y, maxLevel = 2)
# rivers <- importGSHHS("/media/data/ProjectsWork/stat-2014-10-23-Dima/aside/gshhg/gshhg-bin-2.3.3/wdb_rivers_f.b", xlim = Full_x , ylim = Full_y)
# borders <- importGSHHS("/media/data/ProjectsWork/stat-2014-10-23-Dima/aside/gshhg/gshhg-bin-2.3.3/wdb_borders_f.b", xlim = Full_x, ylim = Full_y)
# plotMap(polys, col="beige", bg="lightblue")
# addLines(rivers, col="lightblue")
# addLines(borders, col="red")

#----- General Bathymetric Chart of the Oceans (GEBCO) -----
# GEBCO 30 arc-second grid
library(RNetCDF)
###Data
#data location
bathy_fname <- "/media/data/ProjectsWork/stat-2014-10-23-Dima/aside/GEBCO bathimetry/GEBCO_2014_2D_31.75_66.0_34.0_67.25.nc"
# from https://www.bodc.ac.uk/data/online_delivery/gebco/gebco_08_grid/

#load bathy data
nc <- open.nc(bathy_fname)
print.nc(nc)
tmp <- read.nc(nc)
# z <- array(tmp$z, dim=tmp$dim)
# #z[which(z > 0)] <- NaN
# z <- z[,rev(seq(ncol(z)))]
# xran <- tmp$x_range
# yran <- tmp$y_range
# zran <- tmp$z_range
# lon <- seq(tmp$x[1], tmp$x[2], tmp$spac[1])
# lat <- seq(tmp$y[1], tmp$y[2], tmp$spac[1])
z <- tmp$elevation
lon <- tmp$lon
lat <- tmp$lat
rm(tmp)
close.nc(nc)

###Plot
#make palette
ocean.pal <- colorRampPalette(c("#000000", "#000209", "#000413", "#00061E", "#000728", "#000932", "#002650", "#00426E", "#005E8C", "#007AAA", "#0096C8", "#22A9C2", "#45BCBB", "#67CFB5", "#8AE2AE", "#ACF6A8", "#BCF8B9", "#CBF9CA", "#DBFBDC", "#EBFDED"))

land.pal <- colorRampPalette(c("#336600", "#F3CA89", "#D9A627", "#A49019", "#9F7B0D", "#996600", "#B27676", "#C2B0B0", "#E5E5E5", "#FFFFFF"))

zbreaks <- seq(-250, 500, by=5)
cols <- c(ocean.pal(sum(zbreaks<=0)-1), land.pal(sum(zbreaks>0)))

#compare coastlines to package 'mapdata'
# png("west_pac.png", width=7.5, height=6, units="in", res=200)
#quartz(width=7.5, height=6)
layout(matrix(1:2, 1,2), widths=c(6,1.5), heights=c(6))

par(mar=c(2,2,1,1), ps=10)
image(lon, lat, z=z, col=cols, breaks=zbreaks, useRaster=TRUE, ylim=Small_y, xlim=Small_x)

par(mar=c(2,0,1,5))
image(x=1, y=zbreaks, z=matrix(zbreaks, 1, length(zbreaks)), col=cols, breaks=zbreaks, useRaster=TRUE, xlab="", ylab="", axes=FALSE)
axis(4, at=seq(-250, 500, 375), las=2)
mtext("[meters]", side=4, line=3)
box()

# dev.off()

#----- NOAA bathymetry with marmap -----

library(marmap)
WS <- getNOAA.bathy(lon1 = 31, lon2 = 42, lat1 = 63, lat2 = 68, resolution = 1)
summary(WS)
plot(WS)
plot(WS, image = TRUE)

# Creating a custom palette of blues
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
# Plotting the bathymetry with different colors for land and sea
plot(WS, image = TRUE, land = TRUE, lwd = 0.1, bpal = list(c(0, max(WS), "grey"), c(min(WS), 0, blues)))
