rm(list=ls())

library(ggforce)
library(tidyverse)
library(readxl)
library(scales)


core<-read_excel("./Data/master.xlsx", sheet = "export")


#Creating Descrete Values for x-y axis
Infrastrcture<-core %>%
  select(Warehouse, Office, Trucks, Online_System, Retail, Processing, Shared_Kitchen) %>%
  mutate(sum = rowSums(across(Warehouse:Shared_Kitchen)),
         No_Inf = case_when(sum == 0 ~ 1,
                          TRUE ~ 0)) %>%
  select(-sum)

Business_Model<-core %>%
  mutate(as.data.frame(model.matrix(~ 0 + Business_Model, data=core))) %>%
  select(contains("Business_Model")) %>%
  select(-Business_Model) %>%
  rename_with(.,~ gsub("Business_Model", "", .x))%>%
  rename_with(.,~ gsub(" ", "_", .x))%>%
  rename_with(.,~ gsub("NA", "Unkwn", .x)) %>%
  select(., 1, 3, 2, 4, 6, 5)

Prod_Mix<-core %>%
  mutate(as.data.frame(model.matrix(~ 0 + Product_Mix, data=core))) %>%
  select(contains("Product_Mix")) %>%
  select(-Product_Mix) %>%
  rename_with(.,~ gsub("Product_Mix", "", .x))%>%
  rename_with(.,~ str_to_sentence(gsub("-", "_", .x))) %>%
  select(., 1, 5, 2, 3, 4)

Revenue<-core %>%
  select(contains("size_scale_rev")) %>%
  mutate(rev_class = case_when(size_scale_rev == 0 ~ "Unknown Rev",
                               size_scale_rev <= 1 ~ "Less Than 200K",
                               size_scale_rev == 2 ~ "R200 to 300K",
                               size_scale_rev == 3 ~ "R300 to 400K",
                               size_scale_rev == 4 ~ "R400 to 500K",
                               size_scale_rev == 5 ~ "R500 to 600K",
                               size_scale_rev == 6 ~ "R600 to 700K",
                               size_scale_rev == 7 ~ "R700 to 800K",
                               size_scale_rev == 8 ~ "R800 to 900K",
                               size_scale_rev == 9 ~ "R900k to 1M",
                               size_scale_rev == 10 ~ "Over 1M"),
         as.data.frame(model.matrix(~ 0 + rev_class))) %>%
  select(-c(size_scale_rev, rev_class)) %>%
  rename_with(.,~ gsub("rev_class", "", .x)) %>%
  rename_with(.,~ str_to_sentence(gsub(" ", "_", .x))) %>%
  select(., 7,1,3,4,5,6,2)

Legal_Status<-core %>%
  mutate(as.data.frame(model.matrix(~ 0 + LegStat, data=core))) %>%
  select(contains("LegStat")) %>%
  select(-LegStat) %>%
  rename_with(.,~ gsub("LegStat", "", .x)) %>%
  rename(Cooperative = coop,
         For_Profit_Corporation = corp,
         Not_For_Profit = nonprofit)

Average_Dist <- core %>%
  mutate(as.data.frame(model.matrix(~ 0 + Aver_Dist_M2, data=core))) %>%
  select(contains("Aver_Dist_M2")) %>%
  select(-Aver_Dist_M2) %>%
  rename_with(.,~ gsub("Aver_Dist_M2", "", .x)) %>%
  rename_with(.,~ gsub(" ", "_", .x)) %>%
  select(., 2,3, 4, 5, 1, 6)

Max_Dist <- core %>%
  mutate(as.data.frame(model.matrix(~ 0 + Max_Dist_M2, data=core))) %>%
  select(contains("Max_Dist_M2")) %>%
  select(-Max_Dist_M2) %>%
  rename_with(.,~ gsub("Max_Dist_M2", "", .x)) %>%
  rename_with(.,~ gsub(" ", "_", .x)) 

Small_Source <- core %>%
  mutate(Small2 = paste0("s", as.character(Small*100)),
         mutate(as.data.frame(model.matrix(~ 0 + Small2)))) %>%
  select(contains("Small2"))  %>%
  select(-Small2) %>%
  rename_with(.,~ gsub("Small2", "", .x)) %>%
  rename_with(.,~ gsub(" ", "_", .x)) %>%
  select(.,1, 2, 4, 5, 6, 3)

Medium_Source <- core %>%
  mutate(Med2 = paste0("M", as.character(Medium*100)),
         mutate(as.data.frame(model.matrix(~ 0 + Med2)))) %>%
  select(contains("Med2"))  %>%
  select(-Med2) %>%
  rename_with(.,~ gsub("Med2", "", .x)) %>%
  rename_with(.,~ gsub(" ", "_", .x)) 

Large_Source <- core %>%
  mutate(Lar2 = paste0("L", as.character(Large*100)),
         mutate(as.data.frame(model.matrix(~ 0 + Lar2)))) %>%
  select(contains("Lar2"))  %>%
  select(-Lar2) %>%
  rename_with(.,~ gsub("Lar2", "", .x)) %>%
  rename_with(.,~ gsub(" ", "_", .x))  %>%
  select(., 1, 5, 2, 3, 4, 6, 7)

Household <- core %>%
  mutate(as.data.frame(model.matrix(~ 0 + Household_R))) %>%
  select(contains("Household_R"))  %>%
  select(-Household_R) %>%
  rename_with(.,~ gsub("Household_R", "h", .x)) %>%
  rename_with(.,~ gsub(" ", "_", .x))

Institution <- core %>%
  mutate(as.data.frame(model.matrix(~ 0 + Institutions_R))) %>%
  select(contains("Institutions_R"))  %>%
  select(-Institutions_R) %>%
  rename_with(.,~ gsub("Institutions_R", "i", .x)) %>%
  rename_with(.,~ gsub(" ", "_", .x))

Wholesale <- core %>%
  mutate(as.data.frame(model.matrix(~ 0 + Wholesale_R))) %>%
  select(contains("Wholesale_R"))  %>%
  select(-Wholesale_R) %>%
  rename_with(.,~ gsub("Wholesale_R", "w", .x)) %>%
  rename_with(.,~ gsub(" ", "_", .x))


#Create specific variables to make Clock graphs work.
      
gdata<-core %>%
  mutate(x = 50+(40*(cos(a*(pi/180)))),
         y = 50+(40*(sin(a*(pi/180)))),
         Product_Mix = as.factor(Product_Mix),
         Farm_col = rgb((255*(1-as.numeric(Small)))/255, 
                        (255*(1-as.numeric(Medium)))/255,
                        (255*(1-as.numeric(Large)))/255),
         Farm_Size = paste0("S",Small,"; M",Medium,"; L",Large),
         cyan = ceiling(255 * (1-(Global_Economic/3))),
         magin = ceiling(255 * (1-(Global_Social/3))),
         yell = ceiling(255 * (1-(Global_Environment/3))),
         Miss.Color = rgb(cyan, magin, yell, maxColorValue = 255),
         Miss.Def = paste0(Global_Social, ": Soc; ", 
                           Global_Economic, ": Econ; ",
                           Global_Environment, ": Env")) %>%
  rename(Total_Revenue_Scaled=size_scale_rev)


#Load Clock Graph Function into memory

source("./Build/Code/Clock.R")
source("./Build/Code/Clock2.R") #This is used for the Mission Mix Color Scheme

c1<-clock2(Small_Source, Average_Dist, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Small Farm / Average Distance to Farm")

c2<-clock2(Small_Source, Max_Dist, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Small Farm / Maximum Distance to Farm")

c3<-clock2(Medium_Source, Average_Dist, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Medium Farm / Average Distance to Farm")

c4<-clock2(Medium_Source, Max_Dist, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Medium Farm / Maximum Distance to Farm")

c5<-clock2(Large_Source, Average_Dist, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Large Farm / Average Distance to Farm")

c6<-clock2(Large_Source, Max_Dist, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Large Farm / Maximum Distance to Farm")

c7<-clock2(Small_Source, Prod_Mix, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Small Farm / Product Mix")

c8<-clock2(Medium_Source, Prod_Mix, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Medium Farm / Product Mix")

c9<-clock2(Large_Source, Prod_Mix, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Large Farm / Product Mix")

c10<-clock2(Small_Source, Infrastrcture, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Small Farm / Infrastrcture")

c11<-clock2(Medium_Source, Infrastrcture, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Medium Farm / Infrastrcture")

c12<-clock2(Large_Source, Infrastrcture, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Large Farm / Infrastrcture")

c13<-clock2(Average_Dist, Infrastrcture, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Average Distance to Farm / Infrastrcture")

c14<-clock2(Max_Dist, Infrastrcture, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Maximum Distance to Farm / Infrastrcture")

c15<-clock2(Average_Dist, Prod_Mix, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Average Distance to Farm / Product Mix")

c16<-clock2(Max_Dist, Prod_Mix, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Maximum Distance to Farm / Product Mix")

c17<-clock2(Average_Dist, Business_Model, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Average Distance to Farm / Business Model")

c18<-clock2(Max_Dist, Business_Model, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Maximum Distance to Farm / Business Model")

c17<-clock2(Infrastrcture, Prod_Mix, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Product Mix")

c19<-clock2(Infrastrcture, Business_Model, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Business Model")

c20<-clock2(Infrastrcture, Household, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Sales Point: Households")

c21<-clock2(Infrastrcture, Institution, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Sales Point: Institutions")

c22<-clock2(Infrastrcture, Wholesale, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Sales Point: Wholesale")

c23<-clock2(Average_Dist, Household, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Average Distance to Farm / Sales Point: Households")

c24<-clock2(Average_Dist, Institution, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Average Distance to Farm / Sales Point: Institutions")

c25<-clock2(Average_Dist, Wholesale, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Average Distance to Farm / Sales Point: Wholesale")

c26<-clock2(Max_Dist, Household, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Maximum Distance to Farm / Sales Point: Households")

c27<-clock2(Max_Dist, Institution, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Maximum Distance to Farm / Sales Point: Institutions")

c28<-clock2(Average_Dist, Wholesale, "Miss.Color", "Miss.Def", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Maximum Distance to Farm / Sales Point: Wholesale")


d1<-clock(Infrastrcture, Prod_Mix, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Business Model")

d2<-clock(Infrastrcture, Revenue, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Total Revenue")

d3<-clock(Prod_Mix, Revenue, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Product Mix / Total Revenue")

d4<-clock(Average_Dist, Revenue, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Average Distance to Farm / Total Revenue")

d5<-clock(Max_Dist, Revenue, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Maximum Distance to Farm / Total Revenue")

d6<-clock(Small_Source, Revenue, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Small Farm / Total Revenue")

d7<-clock(Medium_Source, Revenue, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Medium Farm / Total Revenue")

d8<-clock(Large_Source, Revenue, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sourcing: % Large Farm / Total Revenue")

d9<-clock(Infrastrcture, Small_Source, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Sourcing: % Small Farm")

d10<-clock(Infrastrcture, Medium_Source, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Sourcing: % Medium Farm")

d11<-clock(Infrastrcture, Large_Source, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Sourcing: % Large Farm")

d12<-clock(Household, Institution, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Sales Point: Household and Institution")

d13<-clock(Infrastrcture, Household, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Sales Point: Households")

d14<-clock(Infrastrcture, Institution,"Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Sales Point: Institutions")

d15<-clock(Infrastrcture, Wholesale, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Infrastrcture / Sales Point: Wholesale")

d16<-clock(Average_Dist, Household, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Average Distance to Farm / Sales Point: Households")

d17<-clock(Average_Dist, Institution, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Average Distance to Farm / Sales Point: Institutions")

d18<-clock(Average_Dist, Wholesale, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Average Distance to Farm / Sales Point: Wholesale")

d19<-clock(Max_Dist, Household, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Maximum Distance to Farm / Sales Point: Households")

d20<-clock(Max_Dist, Institution, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Maximum Distance to Farm / Sales Point: Institutions")

d21<-clock(Average_Dist, Wholesale, "Business_Model", "LegStat", "Total_Revenue_Scaled", gdata) +
  labs(title="Maximum Distance to Farm / Sales Point: Wholesale")


