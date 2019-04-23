# Required packages -------------------------------------------------------

library(tidyverse)
library(data.table)
library(readxl)
library(gsubfn)
library(lubridate)


# Root folder definition --------------------------------------------------

root <- "C:/Mueller_Jens_Data/Finnmaid_data_compilation/Data"


# 2003 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-2003", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- data.table(
    read_excel(file, skip = 2))
  df <- df[,1:7]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","dTem")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(as.character(df$dTem))
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 1, 8)

  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}

df.all <- temp
rm(df, temp, files, file)



# 2004 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-2004", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- data.table(
    read_excel(file, skip = 2))
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","dTem")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(as.character(df$dTem))
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 1, 8)

  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)



# 2005 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-2005", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- data.table(
    read_excel(file))
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","dTem")
  df <- df[-1,]
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(as.character(df$dTem))
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 1, 8)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)



# 2006 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-2006", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- data.table(
    read_excel(file))
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","dTem")
  df <- df[-1,]
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(as.character(df$dTem))
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 1, 8)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# 2007 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-2007", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- data.table(
    read_excel(file))
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem")
  df <- df[-1,]
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(NA)
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 1, 8)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# 2008 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-2008", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- data.table(
    read_excel(file))
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem", "dTem")
  df <- df[-c(1,2),]
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(as.character(df$dTem))
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# 2009a --------------------------------------------------------------------

setwd(paste(root, "/pCO2-2009a", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- read_excel(file)
  df <- df[c(1,2,3,11,6,4,10,7,5,14)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1,2),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(NA)
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}

df.all <- rbind(df.all, temp)
rm(df, temp, files, file)



# 2009b --------------------------------------------------------------------

setwd(paste(root, "/pCO2-2009b", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- read_excel(file)
  df <- df[c(1,2,3,11,7,4,14,8,5,16)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(NA)
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# 2010 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-2010", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(NA)
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# 2011 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-2011", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(NA)
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# 2012 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-O2-2012", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}

df.all <- rbind(df.all, temp)
rm(df, temp, files, file)



# 2013 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-O2-2013", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# 2014 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-O2-2014", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}

df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# 2015 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-O2-2015", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)

  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)

  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# 2016 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-O2-2016", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)

  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)

  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}

df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# 2017 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-O2-2017", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){

  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)

  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)

  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}

df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# 2018 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-O2-2018", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(2),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


rm(df, files, file)
temp <- temp[pCO2 != 0]


# 2018, data files from Los Gatos Sensor

setwd(paste(root, "/pCO2-O2-2018/LGR", sep=""))
files <- list.files(pattern = "[.]xls$")


for (file in files){

  df <- read_excel(file)
  df <- df[c(2,3,4,8,6,5,14,7,15,9)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- dmy_hms(df$date)
  df <- data.table(df)
  
  df$route <- substr(as.character(file), 12, 12)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp.LGR")){
    temp.LGR <- rbind (temp.LGR, df)
  } else{temp.LGR <- df}
  
}


temp <- rbind(temp, temp.LGR)
rm(temp.LGR, df, file, files)

df.all <- rbind(df.all, temp)
rm(temp)



# 2019 --------------------------------------------------------------------

setwd(paste(root, "/pCO2-O2-2019", sep=""))
files <- list.files(pattern = "[.]xls$")

for (file in files){
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)


# Data quality control ----------------------------------------------------

df.all <- distinct(df.all)
unique(df.all$route)

# lon.int <- 10.9
# param <- "Sal"
# 
# for (lon.int in seq(10,30,1)) {
#   for (param in c("Lat", "pCO2", "Sal", "Tem", "cO2")) {
#     
#     df.all %>%
#       filter(Lon > lon.int, Lon <= lon.int+1) %>%
#       ggplot(aes_string("date", param, col="route"))+
#       geom_point()+
#       scale_x_datetime(date_breaks = "1 year", date_labels = "%Y",
#                        limits = c(ymd_hm("2003-01-01T0001"),
#                                   ymd_hm("2019-12-31T2359")))+
#       theme_bw()
#     
#     setwd("C:/Mueller_Jens_Data/Finnmaid_data_compilation/Plots/Diagnostics")
#     ggsave(paste(param,"_",lon.int,"_degE_timeseries.jpg", sep = ""),
#            width = 15, height = 5)
#     
#     }
# }

df.all <- df.all %>%
  mutate(cO2 = if_else(cO2 < 200, NaN, cO2),
         cO2 = if_else(cO2 > 500 & 
                         date > ymd("2014-07-01") &
                         date < ymd("2014-12-31"), NaN, cO2),
         Sal = if_else(Sal < 4 & year(date) == 2011 & route == "E",
                       NaN, Sal))

# Safe merged data file --------------------------------------------------------------------

setwd(paste(root, "/_summarized_data", sep=""))
write_csv(df.all, "Finnmaid_all_2019.csv")

