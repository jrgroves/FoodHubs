#This pulls in the Analysis data and creates visualization with the larger sample size
#Created: June 13, 2022


rm(list=ls())

library(ggradar)
suppressPackageStartupMessages(library(tidyverse))
library(readxl)



#Read in Data####

  
core<-read_excel("./Data/export.xlsx", sheet = "export")
core$Econ<-as.numeric(core$Econ)
core$Social<-as.numeric(core$Social)
core$Enviro<-as.numeric(core$Enviro)
core$Aver_Dist_M<-as.numeric(core$Aver_Dist_M)


mission.1<-ggplot(data=core)+
  geom_point(aes(x=Econ, y=Stated_Economic, shape=LegStat, color=HubID), size=3)+
  expand_limits(x=c(0,3), y=c(0,3))+
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  scale_colour_discrete(guide = "none") +
  labs(title="Stated vs. Implied Missions", x="Implied Economics", y="Stated Economics",
       shape="Legal Status")+
  theme_bw()

mission.2<-ggplot(data=core)+
  geom_point(aes(x=Social, y=Stated_Social, shape=LegStat, color=HubID), size=3)+
  expand_limits(x=c(0,3), y=c(0,3))+
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  scale_colour_discrete(guide = "none") +
  labs(title="Stated vs. Implied Missions", x="Implied Social", y="Stated Social",
       shape="Legal Status")+
  theme_bw()

mission.3<-ggplot(data=core)+
  geom_point(aes(x = Enviro, y = Stated_Environment, shape = Product_Mix,
                 color = HubID), size = 3)+
  expand_limits(x=c(0,3), y=c(0,3))+
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  scale_colour_discrete(guide = "none") +
  labs(title="Stated vs. Implied Missions", x="Implied Environmental", y="Stated Environmental",
       shape="Legal Status")+
  theme_bw()

core <- core %>%
  mutate(leg=case_when(LegStat == "coop" ~ 1,
                       LegStat == "corp" ~ 2,
                       LegStat == "nonprofit" ~ 3))
library(viridis)

infra.1<-ggplot(data=core, aes(x=reorder(HubID, leg) , y=Sup_Per), fill=LegStat)+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_viridis(discrete=TRUE, name="") 

infra.1<-ggplot(data=core, aes(x=LegStat , y=Sup_Per), fill=LegStat)+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_viridis(discrete=TRUE, name="") 

  geom_vline(aes(xintercept = 8.5))+
  geom_vline(aes(xintercept = 3.5))

infra.2<-ggplot(data=subset(core, LegStat=="coop"), aes(x=HubID))+
  geom_bar(stat = "count")

+
  geom_bar(aes(y=Trucks), stat = "bin") 










#Splits the hubs dataframe into dfs by type
focus<-core %>%
  group_by(LegStat, .add=TRUE) %>%
  group_split()

source("./Build/Code/jradar.R")

f.coop <- focus[[1]] %>%
  select(HubID, Warehouse:Shared_Kitchen)

ggradar(f.coop)
  
f.fp <- focus[[2]] %>%
  select(HubID, Warehouse:Shared_Kitchen)

ggradar(f.fp)

f.nfp <- focus[[3]] %>%
  select(HubID, Warehouse:Shared_Kitchen)
ggradar(f.nfp)


