# Required Packages -------------------------------------------------------

library(seacarb)
library(tidyverse)
library(here)

# Read summarized data file -----------------------------------------------

df <- read_csv(here::here("Data/_summarized_data", "Finnmaid_all_2019.csv"))

# Subarea definition ------------------------------------------------------

df <- df %>% 
  mutate(Area = case_when(
    Lon>12 & Lon<12.6 ~ "1.MEB",
    Lon>13.1 & Lon<14.3 ~ "2.ARK",
    Lat>57.5 & Lat<58.5 & route %in% c("E", "G") ~ "4.EGS",
    Lat>56.8 & Lat<57.5 & route=="W" ~ "3.WGS",
    Lat>58.5 & Lat<59 & Lon>20 ~ "5.NGS",
    Lon>22 & Lon<24 ~ "6.WGF",
    Lon>24 & Lon<24.5 ~ "7.HGF"))

df_mean <- df %>% 
  filter(!is.na(Area)) %>% 
  group_by(Area, ID) %>% 
  summarise_all(list("mean", "sd", "min", "max"), na.rm=TRUE) %>% 
  ungroup() 

df_mean <- df_mean %>% 
  select(-c(date_sd, date_max, date_min,
            Lat_sd, Lat_max, Lat_min,
            Lon_sd, Lon_max, Lon_min))

# df_mean %>% 
#   ggplot(aes(date_mean, Sal_mean, col=Area))+
#   geom_point()


# Fixed salinity and alkalinity data assigned -----------------------------

df_mean <- df_mean %>% 
  mutate(Sal = case_when(
    Area == "1.MEB" ~ 9.6,
    Area == "2.ARK" ~ 7.6,
    Area %in% c("4.EGS", "3.WGS", "5.NGS") ~ 6.7,
    Area == "6.WGF" ~ 6.2,
    Area == "7.HGF" ~ 5.7),
    
    AT.fix = case_when(
    Area == "1.MEB" ~ 1741e-6,
    Area == "2.ARK" ~ 1694e-6,
    Area %in% c("4.EGS", "3.WGS", "5.NGS") ~ 1665e-6,
    Area == "6.WGF" ~ 1580e-6,
    Area == "7.HGF" ~ 1513e-6))



# CT calculation ----------------------------------------------------------

df_mean_CT <- df_mean %>% 
  filter(!is.na(pCO2_mean) & !is.na(Sal_mean) & !is.na(Tem_mean)) %>%
  mutate(CT_mean = carb(24, var1 = pCO2_mean, var2=AT.fix,
                   S=Sal_mean, T=Tem_mean, k1k2="m10", kf="dg", ks="d",
                   pHscale="T", gas="insitu")$DIC*1e6,
         CT_eq_400 = carb(24, var1 = 400, var2=AT.fix,
                   S=Sal_mean, T=Tem_mean, k1k2="m10", kf="dg", ks="d",
                   pHscale="T", gas="insitu")$DIC*1e6)

df_mean <- full_join(df_mean, df_mean_CT)
rm(df_mean_CT)

df_mean %>%
  ggplot(aes(date_mean, CT_mean, col=Area))+
  geom_line()

# Safe data with mean area values and CT calculated -----------------------

write_csv(df_mean,
          here::here("Data/_summarized_data", "Finnmaid_mean_area_CT_2019.csv"))


