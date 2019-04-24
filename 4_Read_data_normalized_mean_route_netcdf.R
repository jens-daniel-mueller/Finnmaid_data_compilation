# Packages ----------------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(here)
library(geosphere)


# Read mean route netcdf from Henry Bittig --------------------------------

nc <- nc_open(here::here("Data/_mean_routes", "Finnmaid_mean_E_W_dist_from_L.nc"))
print(nc)

ncatt_get(nc, 0)
ncatt_get(nc, "lat_E")
ncatt_get(nc, "distx")

nc$var[[1]]
attributes(nc)
attributes(nc$var)
attributes(nc$dim)
nc$nvars

attributes(nc$dim)$names


#var = unlist(attributes(nc$var))[2]

for (var in  unlist(attributes(nc$var))) {
  
  col <- ncvar_get(nc, var)
  
  if (exists("temp")){
  temp <- bind_cols(temp, as_tibble(col))
  } else{temp <- as_tibble(col)}

}

df <- temp %>% 
  set_names(unlist(attributes(nc$var)))

rm(temp)


df <- df %>% 
  mutate(dist = ncvar_get(nc, "distx"),
         dist_geo = distGeo(cbind(lon_E, lat_E), c(10.8605315, 53.9414096))/1e3)


df %>% 
  filter(dist < 200) %>% 
  ggplot()+
  geom_point(aes(lon_W,lat_W, col=dist))+
  geom_point(aes(lon_E,lat_E, col=dist))+
  scale_color_viridis_c()

df %>% 
  ggplot()+
  geom_point(aes(dist, lon_W))+
  geom_point(aes(dist, lon_E))+
  scale_color_viridis_c()

df %>% 
  ggplot()+
  geom_point(aes(dist, dist_geo))
