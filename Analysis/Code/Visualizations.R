#This pulls in the Analysis data and creates visualization with the larger sample size
#Created: June 13, 2022


rm(list=ls())

library(ggradar)
suppressPackageStartupMessages(library(tidyverse))
library(readxl)



#Read in Data####

  
core<-read_excel("./Data/Analysis_Food Hubs.xlsx", sheet = "Data")


#Splits the hubs dataframe into dfs by type
focus<-core %>%
  group_by(Type, .add=TRUE) %>%
  group_split()


#Creates the Radar Plots for Mission Statement#####
f.coop <- focus[[1]] %>%
  select(Type,Economic, Social, Environmental, Community)

f.fp <- focus[[2]] %>%
  select(Type,Economic, Social, Environmental, Community)

f.nfp <- focus[[3]] %>%
  select(Type,Economic, Social, Environmental, Community)

source("./Build/Code/jradar.R")

f.core<-rbind(f.coop, f.fp, f.nfp)

mission.coop<-jradar(f.coop)
mission.fp<-jradar(f.fp)
mission.nfp<-jradar(f.nfp)


#Creates the Radar Plots for Sales Points#####
f.coop <- focus[[1]] %>%
  select(Hub, Consumers, Resturants, Prvt_Inst, Pub_Inst, Wholesale) %>%
  mutate(across(Consumers:Wholesale, ~.x/100))

f.fp <- focus[[2]] %>%
  select(Hub, Consumers, Resturants, Prvt_Inst, Pub_Inst, Wholesale) %>%
  mutate(across(Consumers:Wholesale, ~.x/100))


f.nfp <- focus[[3]] %>%
  select(Hub, Consumers, Resturants, Prvt_Inst, Pub_Inst, Wholesale) %>%
  mutate(across(Consumers:Wholesale, ~.x/100))

sales.coop<-ggradar(f.coop)
sales.fp<-jradar(f.fp)
sales.nfp<-jradar(f.nfp)


#Creates the Radar Plots for Farm Scale#####
f.coop <- focus[[1]] %>%
  select(Hub, Small, Medium, Large) %>%
  mutate(across(Small:Large, ~.x/100))

f.fp <- focus[[2]] %>%
  select(Hub, Small, Medium, Large) %>%
  mutate(across(Small:Large, ~.x/100))


f.nfp <- focus[[3]] %>%
  select(Hub, Small, Medium, Large) %>%
  mutate(across(Small:Large, ~.x/100))

farm.coop<-ggradar(f.coop)
farm.fp<-jradar(f.fp)
farm.nfp<-jradar(f.nfp)

#Creates the Radar Plots for Products#####
f.coop <- focus[[1]] %>%
  select(Hub, Meat:Value_Add) %>%
  mutate(across(Meat:Value_Add, ~.x/100))

f.fp <- focus[[2]] %>%
  select(Hub, Meat:Value_Add) %>%
  mutate(across(Meat:Value_Add, ~.x/100))


f.nfp <- focus[[3]] %>%
  select(Hub, Meat:Value_Add) %>%
  mutate(across(Meat:Value_Add, ~.x/100))

product.coop<-ggradar(f.coop)
product.fp<-jradar(f.fp)
product.nfp<-jradar(f.nfp)

#Creates the Radar Plots for Infrastructure#####
f.coop <- focus[[1]] %>%
  select(Hub, Warehouse:Rental)
  
f.fp <- focus[[2]] %>%
  select(Hub, Warehouse:Rental)

f.nfp <- focus[[3]] %>%
  select(Hub, Meat:Value_Add) 


