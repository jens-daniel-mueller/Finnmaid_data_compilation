# Required Packages -------------------------------------------------------

library(seacarb)
library(tidyverse)
library(lubridate)
library(here)

# Read summarized data file -----------------------------------------------

df <- read_csv(here::here("Data/_summarized_data", "Finnmaid_all_2019.csv"))

# Subarea definition ------------------------------------------------------

df <- df %>%
  filter(year(date) == 2018)


# Test plots --------------------------------------------------------------

df %>% 
  ggplot(aes(Lat, pCO2, col=ID))+
  geom_path()

df %>% 
  filter(Lat > 55, Lat <56) %>% 
  ggplot(aes(date, pCO2, col=Tem))+
  geom_point()

df %>% 
  filter(Lat > 55, Lat <56) %>% 
  ggplot(aes(date, Tem))+
  geom_point()

# Safe data with mean area values and CT calculated -----------------------

write_csv(df,
          here::here("Data/_summarized_data", "Finnmaid_2018.csv"))
