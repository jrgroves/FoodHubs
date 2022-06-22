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



farms<-read_excel("./Data/Analysis_Food Hubs.xlsx", sheet = "Farms")

farms<-farms %>%
  filter(!is.na(Lat)) 

farms.map <- st_as_sf(farms, coords = c("Lon", "Lat"),crs = st_crs(County), agr = "constant")
hubs.map <- st_as_sf(farms, coords = c("Hub_Lon", "Hub_Lat"),crs = st_crs(County), agr = "constant")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#660000", "#0072B2", "#D55E00", "#CC79A7", "#59CC56")

ggplot()+
  geom_sf(data=County, fill="white", color="gray")+
  geom_sf(size=1.25, fill=NA, data=State)+
  geom_sf(size=1.5, data=farms.map) +
  scale_colour_manual(values=cbbPalette)+
  geom_point(aes(Hub_Lon, Hub_Lat, color = Hub), size=4.0, data=farms)+
  geom_segment(aes(x = Lon, y = Lat,
                   xend = Hub_Lon, yend = Hub_Lat, color=Hub),
                   data=farms)+
  xlab("")+
  ylab("")+
  ggtitle("Farm to Hub Connections")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank())


