# Packages ----------------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(here)
library(lubridate)
library(geosphere)

# Subset timeseries by Lat bordes -----------------------------------------

# Read Finnmaid netcdf referenced to mean route

nc <- nc_open(here::here("Data/_mean_routes", "FM_all_2019_on_standard_tracks.nc"))

print(nc)
attributes(nc$var)
attributes(nc$dim)

# read required vectors from netcdf file
route <- ncvar_get(nc, "route")
route <- unlist(strsplit(route, ""))
date_time <- ncvar_get(nc, "time")
latitude_east <- ncvar_get(nc, "latitude_east")


# define selection criteria
select_route <- "E"
low_lat <- 57.5
high_lat <- 58.5


attributes(nc$var)
var_read <- attributes(nc$var)$names[c(9,10,11,12)]
#var_read <- "SST_east"

for (var in var_read) {
  
  array <- ncvar_get(nc, var) # store the data in a 2-dimensional array
  #dim(array) # should have 2 dimensions: 544 coordinate, 2089 time steps
  
  fillvalue <- ncatt_get(nc, var, "_FillValue")
  array[array == fillvalue$value] <- NA
  rm(fillvalue)
  
  #i <- 5
  for (i in seq(1,length(route),1)){
    
    if(route[i] == select_route) {
      slice <- array[i,]
      
      value <- mean(slice[latitude_east > low_lat & latitude_east < high_lat], na.rm = TRUE)
      date <- ymd("2000-01-01") + date_time[i]
      
      temp <- bind_cols(date = date, var=var, value = value)
      
      if (exists("timeseries", inherits = FALSE)){
        timeseries <- bind_rows(timeseries, temp)
      } else{timeseries <- temp}
      
      rm(temp, value, date)
      
    } 
  }
}

nc_close(nc)
rm(list=setdiff(ls(), c("timeseries", "select_route", "low_lat", "high_lat")))

timeseries %>% 
  ggplot(aes(date, value))+
  geom_point()+
  facet_grid(var~., scales = "free_y")
  
timeseries <- timeseries %>% 
  mutate(dataset = "netcdf")



# Read and subset Finnmaid data set provided for netcdf creation ----------

FM <- read_csv(here::here("Data/_summarized_data", "Finnmaid_all_2019.csv"),
               col_types = cols(cO2 = col_double()))

FM_timeseries <- FM %>% 
  filter(route == select_route,
         Lat > low_lat,
         Lat < high_lat) %>% 
  select(ID, date, SSS_east = Sal, SST_east = Tem, pCO2_east = pCO2, oxygen_east = cO2) %>% 
  group_by(ID) %>% 
  summarise_all("mean", na.rm=TRUE) %>% 
  ungroup() %>% 
  select(-ID) %>% 
  gather(key = "var", value = "value", 2:5) %>% 
  mutate(dataset = "provided",
         date = as.Date(date))


df <- bind_rows(FM_timeseries, timeseries)

df %>% 
ggplot()+
  geom_point(aes(date, value, shape=dataset, col=dataset))+
  scale_shape_manual(values = c(3,19))+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~var, scales = "free_y", ncol = 1)+
  theme_bw()+
  labs(title = "Comparison of dataset provided by Jens and netcdf created by Henry",
       subtitle = paste("Latitude interval",low_lat,"-",high_lat,"| route",select_route))

ggsave(here::here("Plots", "timeseries_netcdf_vs_provided.pdf"),
       width = 25, height = 10, dpi = 300)
  
  
  


# Subset sections by date interval ----------------------------------------

# Read Finnmaid netcdf referenced to mean route

nc <- nc_open(here::here("Data/_mean_routes", "FM_all_2019_on_standard_tracks.nc"))

print(nc)
attributes(nc$var)
attributes(nc$dim)

# read required vectors from netcdf file
route <- ncvar_get(nc, "route")
route <- unlist(strsplit(route, ""))
ID <- ncvar_get(nc, "trackid")
date <- ncvar_get(nc, "time")
date <- ymd("2000-01-01") + date
distx <- ncvar_get(nc, "distx")


# define selection criteria
select_route <- "E"
start_date <- ymd("2018-05-03")
end_date <- ymd("2018-05-10")


attributes(nc$var)
var_read <- attributes(nc$var)$names[c(9,11,12)]
#var <- "SST_east"

for (var in var_read) {
  
  array <- ncvar_get(nc, var) # store the data in a 2-dimensional array
  #dim(array) # should have 2 dimensions: 544 coordinate, 2089 time steps
  
  fillvalue <- ncatt_get(nc, var, "_FillValue")
  array[array == fillvalue$value] <- NA
  rm(fillvalue)
  
  #i <- 5
  for (i in seq(1,length(date),1)){
    
    if(date[i] > start_date & date[i] < end_date) {
      slice <- array[i,]
      
      temp <- tibble(ID = ID[i], var=var, distx = distx, value = slice)
      
      if (exists("sections", inherits = FALSE)){
        sections <- bind_rows(sections, temp)
      } else{sections <- temp}
      
      rm(temp, slice)
      
    } 
  }
}

nc_close(nc)
rm(list=setdiff(ls(), c("sections", "select_route", "start_date", "end_date")))

sections %>% 
  ggplot(aes(distx, value))+
  geom_line()+
  #scale_color_viridis_c()+
  facet_grid(var~ID, scales = "free_y", labeller = label_both)
  
sections <- sections %>% 
  mutate(dataset = "netcdf")



# Read and subset Finnmaid data set provided for netcdf creation ----------

FM <- read_csv(here::here("Data/_summarized_data", "Finnmaid_all_2019.csv"),
               col_types = cols(cO2 = col_double()))

FM_sections <- FM %>% 
  filter(route == select_route,
         as.Date(date) > start_date,
         as.Date(date) < end_date) %>% 
  select(ID, SST_east = Tem, pCO2_east = pCO2, oxygen_east = cO2, Lon, Lat) %>% 
  mutate(distx = distGeo(cbind(Lon, Lat), c(10.8605315, 53.9414096))/1e3) %>% 
  gather(key = "var", value = "value", 2:4) %>% 
  select(ID, var, distx, value) %>% 
  mutate(dataset = "provided")


df <- bind_rows(FM_sections, sections)

# df_na <- df %>% 
#   filter(!complete.cases(df))
# 
# df <- df %>% 
#   drop_na()


df %>%
  ggplot()+
  geom_point(aes(distx, value, col=dataset))+
  geom_line(aes(distx, value, col=dataset))+
  scale_shape_manual(values = c(3,19))+
  scale_color_brewer(palette = "Set1")+
  facet_grid(var~ID, scales = "free_y")+
  theme_bw()+
  labs(title = "Comparison of dataset provided by Jens and netcdf created by Henry",
       subtitle = paste("Time interval",start_date,"-",end_date,"| route",select_route))

ggsave(here::here("Plots", "sections_netcdf_vs_provided.pdf"),
       width = 25, height = 10, dpi = 300)
  
  
df %>%
  filter(distx > 720, distx < 750) %>% 
  ggplot()+
  geom_point(aes(distx, value, col=dataset))+
  geom_line(aes(distx, value, col=dataset))+
  scale_shape_manual(values = c(3,19))+
  scale_color_brewer(palette = "Set1")+
  facet_grid(var~ID, scales = "free_y")+
  theme_bw()+
  labs(title = "Comparison of dataset provided by Jens and netcdf created by Henry",
       subtitle = paste("Time interval",start_date,"-",end_date,"| route",select_route))

ggsave(here::here("Plots", "sections_netcdf_vs_provided_zoom.pdf"),
       width = 25, height = 10, dpi = 300)
  
  
  
  
  
  
  
  
  