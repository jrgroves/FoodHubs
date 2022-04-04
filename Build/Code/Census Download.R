library(tidycensus)
library(tidyverse)
library(sf)
library(readxl)

#Functions


#Generate data from Census API
#Pre-defining variables to be used in loop

vars<-c("B01001_001")
states<-c("indiana","illinois","missouri","wisconsin","iowa","michigan","minnesota") 
fips<-c(18,17,29,55,19,26,27)

#API Command
k<-1

for(i in fips){
  acs<-get_acs(geography="county",
               variables = vars, 
               state = i,
               year  =  2020, 
               geometry  =  TRUE)
  temp<-acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value=estimate) %>%
    mutate(state = states[k])
  
  assign(paste0(states[k],"census"),temp)
  
  temp$area<-st_area(temp)
  map <- temp %>%
    mutate(state = states[k])
  
  assign(paste0(states[k],"map_co"),map) 
  
  temp$area<-st_area(temp)
  map <- temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  
  assign(paste0(states[k],"map_st"),map) 
  
  k<-k+1
  rm(temp, map)
}

State<-rbind(illinoismap_st,iowamap_st,michiganmap_st,wisconsinmap_st,missourimap_st,
              indianamap_st,minnesotamap_st)

County<-rbind(illinoismap_co,iowamap_co,michiganmap_co,wisconsinmap_co,missourimap_co,
              indianamap_co,minnesotamap_co)



farms<-read_excel("./Build/Input/Responses.xlsx", sheet = "Farms")

farms<-farms %>%
  filter(!is.na(Lat))

farms <- st_as_sf(farms, coords = c("Lon", "Lat"),crs = 4326, agr = "constant")


hubs<-read_excel("./Build/Input/Responses.xlsx", sheet = "Hubs")

hubs<-hubs %>%
  filter(!is.na(Lat))

hubs <- st_as_sf(hubs, coords = c("Lon", "Lat"),crs = 4326, agr = "constant")


map<-ggplot()+
  geom_sf(
    data = County,
    fill=NA, colour="gray",
    size=1,
    inherit.aes=FALSE)+
  geom_sf(
    data = State,
    fill=NA, colour="black",
    size=1,
    inherit.aes=FALSE)+
  geom_sf(data = farms, 
          size = 1, 
          shape = 23, 
          fill = "darkred")+
  geom_sf(data = hubs, 
          size = 4, 
          shape = 10, 
          fill = "navy") 
