#This file pulls in the Hubs and Farms and also the Census Tract maps to map the Hubs and Farms to census tract.
#This also creates the point and spoke maps for each HUB.

rm(list=ls())

library(tidycensus)
library(tidyverse)
library(sf)
library(readxl)
options(tigris_use_cache = TRUE)

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

#Tracts<-rbind(illinoiscensus,iowacensus,michigancensus,wisconsincensus,missouricensus,
#            indianacensus,minnesotacensus)

State<-rbind(illinoismap_st,iowamap_st,michiganmap_st,wisconsinmap_st,missourimap_st,
              indianamap_st,minnesotamap_st)

County<-rbind(illinoismap_co,iowamap_co,michiganmap_co,wisconsinmap_co,missourimap_co,
              indianamap_co,minnesotamap_co)

farms<-read_excel("./Build/Input/Responses.xlsx", sheet = "Farms")

farms<-farms %>%
  filter(!is.na(Lat)) %>%
  mutate(lon1=Lon,
         lat1=Lat)

farms.map <- st_as_sf(farms, coords = c("Lon", "Lat"),crs = st_crs(County), agr = "constant")


hubs<-read_excel("./Build/Input/Responses.xlsx", sheet = "Hubs")

hubs<-hubs %>%
  filter(!is.na(Lat)) %>%
  mutate(lon1=Lon,
         lat1=Lat)

hubs.map <- st_as_sf(hubs, coords = c("Lon", "Lat"),crs = st_crs(County), agr = "constant")

temp<-hubs.map %>%
  select(HUB)
hub.int<-st_intersection(temp, Tracts)

hub.int<-hub.int %>%
  mutate(State=substr(GEOID,1,2),
         County=substr(GEOID,3,5),
         Tract=substr(GEOID,6,11)) %>%
  select(!c(state,NAME))


temp<-farms.map %>%
  select(Name)
farm.int<-st_intersection(temp, Tracts)

farm.int<-farm.int %>%
  mutate(State=substr(GEOID,1,2),
       County=substr(GEOID,3,5),
       Tract=substr(GEOID,6,11)) %>%
  select(!c(state,NAME))



#Spoke Hub Graphics


sub<-hubs.map %>%
  filter(HUB=="Fifth Season")
sub.farm<-farms.map %>%
  filter(HUB=="Fifth Season")


ggplot()+
  geom_sf(
    data = subset(County, state=="wisconsin" | state=="minnesota"),
    fill=NA, colour="gray",
    size=1,
    inherit.aes=FALSE)+
  geom_sf(
    data = subset(State, state=="wisconsin" | state=="minnesota"),
    fill=NA, colour="black",
    size=1,
    inherit.aes=FALSE)+
  geom_sf(data = sub.farm, 
          size = 1, 
          shape = 23, 
          fill = "darkred")+
  geom_sf(data = sub, 
          size = 4, 
          shape = 25, 
          fill = "navy") +
  geom_segment(aes(x = sub$lon1, y = sub$lat1,
                   xend = sub.farm$lon1, yend = sub.farm$lat1,
                   colour = "red"))+
  theme_minimal()+
  theme(legend.position = "none")


ggplot()+
  geom_sf(data = County,
    fill=NA, colour="gray",
    size=1,
    inherit.aes=FALSE)+
  geom_sf(data = State,
    fill=NA, colour="black",
    size=1,
    inherit.aes=FALSE)+
  geom_sf(data = farms.map, 
          size = 1, 
          shape = 21, 
          fill = "darkred")+
  geom_sf(data = hubs.map, 
          size = 3, 
          shape = 25, 
          fill = "navy")+
  theme_bw()


