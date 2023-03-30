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


############################################
#class notes 03/30/2023

bath_m_df = marmap::fortify.bathy(bath_m_raw)
head(bath_m_df)
bath_m = bath_m_df %>%
  mutate(depth_m = ifelse(z>20, NA, z)) %>%
  dplyr::select(-z)
head(bath_m)
tail(bath_m)


#gulf of maine
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)



ggplot()+
  geom_raster(data=bath_m, aes(x=x, y=y, fill=depth_m)) +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_fixed(1.3, xlim=lon_bounds, ylim=lat_bounds, expand = FALSE) +
  scale_fill_gradientn(colors=c("black", "darkblue", "lightblue"), 
                       values = scales::rescale(c(-6000, -300, 0)), 
                       name = "Depth (m)")

#contour lines
ggplot()+
  geom_raster(data=bath_m, aes(x=x, y=y, fill=depth_m))+
  geom_contour(data=bath_m, aes(x=x, y=y, z=depth_m), 
               breaks=c(-100), linewidth=c(0.25), color="grey")+
  geom_contour(data=bath_m, aes(x=x, y=y, z=depth_m), 
               breaks=c(-200), linewidth=c(0.5), color="grey")+
  geom_contour(data=bath_m, aes(x=x, y=y, z=depth_m), 
               breaks=c(-500), linewidth=c(0.75), color="grey")+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_fixed(1.3, xlim=lon_bounds, ylim=lat_bounds, expand = FALSE)+ 
  theme_bw()


#rasterizing the bathy object
bath_m_raster = marmap::as.raster(bath_m_raw)
bath_m_raster

names(bath_m_raster) = "bath_m"

#resample 
bath_layer_chl_dims = raster::resample(bath_m_raster, chl_GOM_raster)
bath_layer_chl_dims

raster_stack= stack(chl_GOM_raster, bath_layer_chl_dims)
raster_stack

plot(raster_stack)

stack_df= data.frame(raster::rasterToPoints(raster_stack))
head(stack_df)


#O'Reilly
oligo_chl_a = 0.1
eutro_chl_a = 1.67

stack_df = stack_df %>%
  mutate(trophic_index = case_when(chl_a < oligo_chl_a ~"oligotrophic", 
         chl_a > oligo_chl_a & chl_a <eutro_chl_a ~ "mesotrophic", 
         chl_a > eutro_chl_a ~ "eutrophic"))%>%
  mutate(trophic_index = as.factor(trophic_index))
head(stack_df)
tail(stack_df)
summary(stack_df)

ggplot() +
  geom_raster(data=stack_df, aes(x=x, y=y, fill=trophic_index))+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="black") +
  coord_fixed(1.3, xlim=lon_bounds, ylim=lat_bounds, expand = FALSE)+ 
  theme_bw()

stack_df %>%
  group_by(trophic_index) %>%
  summarize(mean_bath_m = mean(bath_m))


library(sf)

USA_crit_hab = st_read(dsn = "data/North_Atlantic_Right_Whale_Critical_Habitat/", 
                       layer = "North_Atlantic_Right_Whale_Critical_Habitat")

USA_crit_hab_sf = st_transform(USA_crit_hab, crs=4326)

CAN_crit_hab = read.csv("data/NARW_canadian_critical_habitat_2017.csv")
head(CAN_crit_hab)

CAN_crit_hab_sf = CAN_crit_hab %>%
  st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
  dplyr::group_by(habitat, country) %>%
  dplyr::summarize(do_union=FALSE) %>%
  st_cast("POLYGON")

CAN_crit_hab_sf

USA_crit_hab_sf$habitat = c("GOM", "SEUS")
USA_crit_hab_sf$country="USA"


USA_crit_hab_sf = USA_crit_hab_sf %>%
  dplyr::select(country, habitat, geometry)

crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)
crit_hab










