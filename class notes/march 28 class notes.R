#2023-03-28
#class notes unit 5 day 1

library(tidyverse)
library(raster)  #threw a warning 
library(mapdata) #threw a warning
library(marmap) #bathymetry data supplied by NOAA = getNOAA.bathy()


chl_raster = raster("data/AQUA_MODIS.20020701_20220731.Lm3.MC.CHL.chlor_a.9km.nc")  #typed in myself, didn't work

chl_raster = raster('data/AQUA_MODIS.20020701_20220731.L3m.MC.CHL.chlor_a.9km.nc')  #copy and pasted from tutorial, worked


chl_raster
names(chl_raster) = "chl_a"
names(chl_raster)

#changing to data frame
chl_pts = rasterToPoints(chl_raster, spatial = TRUE)
head(chl_pts)
chl_df = data.frame(chl_pts)
head(chl_df)

hist(chl_df$chl_a)
max(chl_df$chl_a)
hist(log10(chl_df$chl_a))

# colors? 
cols = rainbow(7, rev=TRUE)[-1]

global_chl_map = ggplot() +
  geom_raster(data=chl_df, aes(x=x, y=y, fill=log10(chl_a)))+
  scale_fill_gradientn(colors=cols, limit=c(-1.5, 0.75), name="log_10(chl_a)")+
  theme_classic()
ggsave(global_chl_map, filename="figures/global_chl_July2002-July2022.pdf", 
       device="pdf", height=5, width=9)

#gulf of maine
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

#crop
c(lon_bounds, lat_bounds)
chl_GOM_raster = raster::crop(chl_raster, extent(c(lon_bounds, lat_bounds)))
chl_GOM_raster

chl_GOM_df = data.frame( rasterToPoints(chl_GOM_raster, spatial = TRUE) )
head(chl_GOM_df)

world_map = map_data("worldHires")
head(world_map)

GOM_chl_map = ggplot() +
  geom_raster(data=chl_GOM_df, aes(x=x, y=y, fill=log10(chl_a)))+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="darkgrey")+
  scale_fill_gradientn(colors=cols, limits=c(-1, 1.75), ) + 
  theme_bw()+
  coord_fixed(1.3, xlim=lon_bounds, ylim=lat_bounds, expand=FALSE)
GOM_chl_map  


#bathymetry
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1], 
                      lon2 = lon_bounds[2], 
                      lat1 = lat_bounds[1], 
                      lat2 = lat_bounds[2], 
                      resolution = 4) #in arcminutes
class(bath_m_raw)

