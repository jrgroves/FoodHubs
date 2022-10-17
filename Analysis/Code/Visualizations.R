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

mission.coop<-ggsave("miss_coop.png", jradar(f.coop), device = "png", path="./Analysis/Output/", dpi = 600)
mission.fp<-ggsave("miss_fp.png", jradar(f.fp), device = "png", path="./Analysis/Output/", dpi = 600)
mission.nfp<-ggsave("miss_nfp.png", jradar(f.nfp), device = "png", path="./Analysis/Output/", dpi = 600)


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

sales.coop<-ggsave("sales_coop.png", ggradar(f.coop), device = "png", path="./Analysis/Output/", dpi = 600)
sales.fp<-ggsave("sales_fp.png", ggradar(f.fp), device = "png", path="./Analysis/Output/", dpi = 600)
sales.nfp<-ggsave("sales_np.png", ggradar(f.nfp), device = "png", path="./Analysis/Output/", dpi = 600)


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

farm.coop<-ggsave("farm_coop.png", ggradar(f.coop), device = "png", path="./Analysis/Output/", dpi = 600)
farm.fp<-ggsave("farm_fp.png", ggradar(f.fp), device = "png", path="./Analysis/Output/", dpi = 600)
farm.nfp<-ggsave("farm_nfp.png", ggradar(f.nfp), device = "png", path="./Analysis/Output/", dpi = 600)

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

product.coop<-ggsave("product_coop.png", ggradar(f.coop), device = "png", path="./Analysis/Output/", dpi = 600)
product.fp<-ggsave("product_fp.png", ggradar(f.fp), device = "png", path="./Analysis/Output/", dpi = 600)
product.nfp<-ggsave("product_nfp.png", ggradar(f.nfp), device = "png", path="./Analysis/Output/", dpi = 600)

#Creates the Radar Plots for Infrastructure#####


f.coop <- focus[[1]] %>%
  select(Hub, Warehouse:Rental)
  
f.fp <- focus[[2]] %>%
  select(Hub, Warehouse:Rental)

f.nfp <- focus[[3]] %>%
  select(Hub, Meat:Value_Add) 


