#This file pulls in the Farm Hub Data (Analysis_Farm Hub.xlsx) for pre-processing for visualizations


rm(list=ls())

library(tidyverse)

#Functions####

#Data Input####

core <- read.csv(file="./Data/Analysis.csv")

core <- core %>%
  filter(!is.na(Year_St)) %>%
  select(Hub.Name, Employees, Type, Sales, Year_St,
         Economic, Social, Environmental, Community,
         Consumers, Resturants, Prvt_Institution, Pub_Institution, Wholesale,
         Small, Medium, Large,
         Meat, Dairy, Fruits_Vegitables, Other, Eggs, Value_Added,
         Warehouse, Office, Trucks, Online.System, Processing, Retail, Rental)

save(core, file="./Build/Output/core.RData")
