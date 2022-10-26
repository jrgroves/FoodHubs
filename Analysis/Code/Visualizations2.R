#This pulls in the Analysis data and creates visualization with the larger sample size
#Created: October 15, 2022


rm(list=ls())

suppressPackageStartupMessages(library(tidyverse))
library(readxl)
library(reshape2)


#Read in Data####

  
core<-read_excel("./Data/export.xlsx", sheet = "export")

core<-core %>%
  mutate(Economic = as.numeric(Economic),
         Social = as.numeric(Social),
         Environmental = as.numeric(Environmental),
         Aver_Dist_M = as.numeric(Aver_Dist_M),
         BOD = as.numeric(BOD),
         Pres_CEO = as.numeric(Pres_CEO),
         Manager = as.numeric(Manager),
         Staff = as.numeric(Staff),
         leg=case_when(LegStat == "coop" ~ "Cooperative",
                       LegStat == "corp" ~ "For-Profit Corporation",
                       LegStat == "nonprofit" ~ "Not-for-Profit Corporation"))
legal<-core %>%
  select(HubID, leg)
pmix<-core %>%
  select(HubID, Product_Mix)

st_miss <- core %>%
  select(HubID, starts_with("Stated"))


#Organizational Status

org.1<-ggplot(data=legal, aes(x=leg))+
  geom_bar(stat="count", fill=c("#CC6666", "#9999CC", "#66CC99")) +
  scale_fill_brewer()+
  labs(title = "Hub Legal Structures", x="Legal Structres", y="Number of Food Hubs")+
  theme_bw()+theme(axis.text.x=element_text(size=22, angle=0, vjust=0.3),
                   axis.title.x=element_text(size = 26, face = "bold", vjust=-0.75),
                   axis.title.y=element_text(size = 26, face = "bold"),
                   axis.text.y=element_text(size=22),
                   plot.title=element_text(size=28, face="bold"),
                   strip.text.x = element_text(size = 20))

core.org<-core %>%
  select(HubID, BOD:Staff) %>%
  replace(is.na(.), 0) %>%
  data.matrix() 
rownames(core.org)<-core$HubID
core.org<-core.org[,-1]
core.org<-melt(core.org)

core.org2<-core.org %>%
  rename("HubID" = "Var1") %>%
  inner_join(y=legal, by="HubID") %>%
  inner_join(y=pmix, by="HubID") %>%
  inner_join(y=st_miss, by="HubID")

org.2<-ggplot(core.org2, aes(x = HubID, y = Var2)) + 
  geom_raster(aes(fill=value)) + 
  geom_hline(yintercept = c(0.5, 1.5, 2.5, 3.5, 4.5), size = 1)+
  geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), size = 1)+
  facet_wrap(~ leg, scale="free_x")+
  scale_fill_gradientn(colors=c("White","#9999FF"),
                       limits = c(0, 1),
                       breaks = c(0,1), labels=format(c(0,1)),
                       name = "Social Mission")+
  labs(x="Food Hubs", y="Decision Makers", title="Food Hub Decision Makers")+
  theme_bw()+theme(axis.text.x=element_text(size=22, angle=0, vjust=0.3),
                   axis.title.x=element_text(size = 26, face = "bold", vjust=-0.75),
                   axis.title.y=element_text(size = 26, face = "bold"),
                   axis.text.y=element_text(size=26),
                   plot.title=element_text(size=28, face="bold"),
                   axis.line = element_blank(),
                   panel.grid = element_blank(),
                   legend.position = "bottom",
                   legend.key.size = unit(2, 'cm'),
                   legend.title = element_text(size=26),
                   legend.text = element_text(size=22),
                   strip.text.x = element_text(size = 20))

org.3<-ggplot(data=filter(core.org2, value!=0), aes(x = HubID, y = Var2)) + 
              geom_raster(aes(fill=Product_Mix)) + 
              geom_hline(yintercept = c(0.5, 1.5, 2.5, 3.5, 4.5), size = 1)+
              geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), size = 1)+
              facet_wrap(~ leg, scale="free_x")+
              labs(x="Food Hubs", y="Decision Makers", title="Food Hub Decision Makers",
                   fill = "Product Mixture")+
              theme_bw()+theme(axis.text.x=element_text(size=22, angle=0, vjust=0.3),
                               axis.title.x=element_text(size = 26, face = "bold", vjust=-0.75),
                               axis.title.y=element_text(size = 26, face = "bold"),
                               axis.text.y=element_text(size=26),
                               plot.title=element_text(size=28, face="bold"),
                               axis.line = element_blank(),
                               panel.grid = element_blank(),
                               legend.position = "bottom",
                               legend.key.size = unit(2, 'cm'),
                               legend.title = element_text(size=26),
                               legend.text = element_text(size=22),
                               strip.text.x = element_text(size = 20))

org.4<-ggplot(data=filter(core.org2, value!=0), aes(x = HubID, y = Var2)) + 
        geom_raster(aes(fill=Stated_Environment)) + 
        geom_hline(yintercept = c(0.5, 1.5, 2.5, 3.5, 4.5), size = 1)+
        geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), size = 1)+
        facet_wrap(~ leg, scale="free_x")+
        labs(x="Food Hubs", y="Decision Makers", title="Food Hub Decision Makers",
             fill = "Importance of Environmental Mission")+
        theme_bw()+theme(axis.text.x=element_text(size=22, angle=0, vjust=0.3),
                         axis.title.x=element_text(size = 26, face = "bold", vjust=-0.75),
                         axis.title.y=element_text(size = 26, face = "bold"),
                         axis.text.y=element_text(size=26),
                         plot.title=element_text(size=28, face="bold"),
                         axis.line = element_blank(),
                         panel.grid = element_blank(),
                         legend.position = "bottom",
                         legend.key.size = unit(2, 'cm'),
                         legend.title = element_text(size=26),
                         legend.text = element_text(size=22),
                         strip.text.x = element_text(size = 20))


#Type of Food Hubs
core.1<-core %>%
  select(HubID, Online_System:Retail, Warehouse:Shared_Kitchen) %>%
  data.matrix()
rownames(core.1)<-core$HubID
core.1<-core.1[,-1]

core.2<-melt(core.1)

core.2<-core.2 %>%
  rename("HubID" = "Var1") %>%
  inner_join(y=legal, by="HubID") %>%
  inner_join(y=pmix, by="HubID") %>%
  inner_join(y=st_miss, by="HubID")




infra.1<-ggplot(filter(core.2, value!=0), aes(x = HubID, y = Var2)) + 
  geom_raster(aes(fill=Stated_Economic)) + 
  scale_fill_gradientn(colors=c("gray", "#FF6666",  "navyblue"),
                       limits = c(0, 3),
                       breaks = c(0,1,2,3), labels=format(c(0,1,2,3)),
                       name = "Economic Mission")+
  geom_hline(yintercept = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5), size = 1)+
  geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), size = 1)+
  facet_wrap(~ leg, scale="free_x")+
  labs(x="Food Hubs", y="Infrastructure", title="Food Hub Infrastructure")+
  theme_bw()+theme(axis.text.x=element_text(size=22, angle=0, vjust=0.3),
                   axis.title.x=element_text(size = 26, face = "bold", vjust=-0.75),
                   axis.title.y=element_text(size = 26, face = "bold"),
                   axis.text.y=element_text(size=26),
                   plot.title=element_text(size=28, face="bold"),
                   axis.line = element_blank(),
                   panel.grid = element_blank(),
                   legend.position = "bottom",
                   legend.key.size = unit(2, 'cm'),
                   legend.title = element_text(size=26),
                   legend.text = element_text(size=22),
                   strip.text.x = element_text(size = 20))


infra.2<-ggplot(filter(core.2, value!=0), aes(x = HubID, y = Var2)) + 
                geom_raster(aes(fill=Stated_Social)) + 
                scale_fill_gradientn(colors=c("gray", "#FF6666",  "navyblue"),
                                     limits = c(0, 3),
                                     breaks = c(0,1,2,3), labels=format(c(0,1,2,3)),
                                     name = "Social Mission")+
                geom_hline(yintercept = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5), size = 1)+
                geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), size = 1)+
                facet_wrap(~ leg, scale="free_x")+
                labs(x="Food Hubs", y="Infrastructure", title="Food Hub Infrastructure")+
                theme_bw()+theme(axis.text.x=element_text(size=22, angle=0, vjust=0.3),
                   axis.title.x=element_text(size = 26, face = "bold", vjust=-0.75),
                   axis.title.y=element_text(size = 26, face = "bold"),
                   axis.text.y=element_text(size=26),
                   plot.title=element_text(size=28, face="bold"),
                   axis.line = element_blank(),
                   panel.grid = element_blank(),
                   legend.position = "bottom",
                   legend.key.size = unit(2, 'cm'),
                   legend.title = element_text(size=26),
                   legend.text = element_text(size=22),
                   strip.text.x = element_text(size = 20))


infra.3<-ggplot(filter(core.2, value!=0), aes(x = HubID, y = Var2)) + 
                geom_raster(aes(fill=Stated_Environment)) + 
                scale_fill_gradientn(colors=c("gray", "#FF6666",  "navyblue"),
                                     limits = c(0, 3),
                                     breaks = c(0,1,2,3), labels=format(c(0,1,2,3)),
                                     name = "Environmental Mission")+
                geom_hline(yintercept = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5), size = 1)+
                geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), size = 1)+
                facet_wrap(~ leg, scale="free_x", )+
                labs(x="Food Hubs", y="Infrastructure", title="Food Hub Infrastructure")+
                theme_bw()+theme(axis.text.x=element_text(size=22, angle=0, vjust=0.3),
                                 axis.title.x=element_text(size = 26, face = "bold", vjust=-0.75),
                                 axis.title.y=element_text(size = 26, face = "bold"),
                                 axis.text.y=element_text(size=26),
                                 plot.title=element_text(size=28, face="bold"),
                                 axis.line = element_blank(),
                                 panel.grid = element_blank(),
                                 legend.position = "bottom",
                                 legend.key.size = unit(2, 'cm'),
                                 legend.title = element_text(size=26),
                                 legend.text = element_text(size=22),
                                 strip.text.x = element_text(size = 20))

infra.4<-ggplot(filter(core.2, value!=0), aes(x = HubID, y = Var2)) + 
                geom_raster(aes(fill=Product_Mix)) + 
                geom_hline(yintercept = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5), size = 1)+
                geom_vline(xintercept = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), size = 1)+
                facet_wrap(~ leg, scale="free_x")+
                labs(x="Food Hubs", y="Infrastructure", title="Food Hub Infrastructure",
                     fill="Product Mixture")+
                theme_bw()+theme(axis.text.x=element_text(size=22, angle=0, vjust=0.3),
                                 axis.title.x=element_text(size = 26, face = "bold", vjust=-0.75),
                                 axis.title.y=element_text(size = 26, face = "bold"),
                                 axis.text.y=element_text(size=26),
                                 plot.title=element_text(size=28, face="bold"),
                                 axis.line = element_blank(),
                                 panel.grid = element_blank(),
                                 legend.position = "bottom",
                                 legend.key.size = unit(2, 'cm'),
                                 legend.title = element_text(size=26),
                                 legend.text = element_text(size=22),
                                 strip.text.x = element_text(size = 20))

#Where do you sale and where do you get?

core.3<-core %>%
  select(HubID, Households:Wholesale, Small:Large) %>%
  mutate( Households = Households /100,
          Institutions = Instructions/100,
          Wholesale = Wholesale / 100) %>%
  select(!Instructions) %>%
  pivot_longer(-HubID, names_to = "Case", values_to = "Value") %>%
  inner_join(y=legal, by="HubID") %>%
  inner_join(y=pmix, by="HubID") %>%
  inner_join(y=st_miss, by="HubID")

farms<-ggplot(data= filter(core.3, Case=="Small" | Case=="Medium" | Case=="Large"), aes(x=HubID, y=Value, fill=Case)) +
          geom_bar(position="stack", stat="identity") +
          facet_wrap(~ leg, scale="free_x") +
          scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))+
          labs(x="Food Hubs", y="Shares", title="From Where Do We Get?",
               fill="Farms")+
          theme_bw()+theme(axis.text.x=element_text(size=22, angle=0, vjust=0.3),
                           axis.title.x=element_text(size = 26, face = "bold", vjust=-0.75),
                           axis.title.y=element_text(size = 26, face = "bold"),
                           axis.text.y=element_text(size=26),
                           plot.title=element_text(size=28, face="bold"),
                           axis.line = element_blank(),
                           panel.grid = element_blank(),
                           legend.position = "bottom",
                           legend.key.size = unit(2, 'cm'),
                           legend.title = element_text(size=26),
                           legend.text = element_text(size=22),
                           strip.text.x = element_text(size = 20))

sales.1<- ggplot(data = filter(core.3, Case!="Small" & Case!="Medium" & Case!="Large"), aes(x=HubID, y=Value, fill=Case)) +
                geom_bar(position="stack", stat="identity") +
                facet_wrap(~ leg, scale="free_x") +
                scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))+
                labs(x="Food Hubs", y="Sale Share", title="To Whom Do We Sell?",
                     fill="Sales Point")+
                theme_bw()+theme(axis.text.x=element_text(size=22, angle=0, vjust=0.3),
                                 axis.title.x=element_text(size = 26, face = "bold", vjust=-0.75),
                                 axis.title.y=element_text(size = 26, face = "bold"),
                                 axis.text.y=element_text(size=26),
                                 plot.title=element_text(size=28, face="bold"),
                                 axis.line = element_blank(),
                                 panel.grid = element_blank(),
                                 legend.position = "bottom",
                                 legend.key.size = unit(2, 'cm'),
                                 legend.title = element_text(size=26),
                                 legend.text = element_text(size=22),
                                 strip.text.x = element_text(size = 20))

#Mission Comparisons

core.4<-core%>%
  select(HubID, Economic:Stated_Economic)%>%
  inner_join(y=legal, by="HubID") %>%
  inner_join(y=pmix, by="HubID") %>%
  filter(!is.na(Economic))


mission.1<-ggplot(data=core.4)+
  geom_point(aes(x=Economic, y=Stated_Economic, shape = leg, color = HubID), size = 10,
             position = position_jitter(h=0.05, w=0.05), alpha = .5)+
  xlim(0,3.25) +
  ylim(0,3.25) +
  geom_abline(intercept = 0, slope = 1,
              linetype="dashed", size = 1) +
  scale_colour_discrete(guide = "none") +
  labs(title="Survey vs. Published Mission Focus", x="Survey Average", y="Published Ranking",
       shape="Legal Status")+
  theme_bw()+theme(axis.text.x=element_text(size=22, angle=0, vjust=0.3),
                   axis.title.x=element_text(size = 26, face = "bold", vjust=-0.75),
                   axis.title.y=element_text(size = 26, face = "bold"),
                   axis.text.y=element_text(size=26),
                   plot.title=element_text(size=28, face="bold"),
                   axis.line = element_blank(),
                   panel.grid = element_blank(),
                   legend.position = "bottom",
                   legend.key.size = unit(2, 'cm'),
                   legend.title = element_text(size=26),
                   legend.text = element_text(size=22),
                   strip.text.x = element_text(size = 20),
                   aspect.ratio = 1)


mission.2<-ggplot(data=core)+
  geom_point(aes(x=Social, y=Stated_Social, shape=leg, color=HubID), size=5,
             position = position_jitter(h=0.05, w=0.05), alpha = 0.5)+
  expand_limits(x=c(0,3), y=c(0,3))+
  geom_abline(intercept = 0, slope = 1,
              linetype="dashed", size = 1) +
  scale_colour_discrete(guide = "none") +
  labs(title="Stated vs. Implied Missions", x="Implied Social", y="Stated Social",
       shape="Legal Status")+
  theme_bw()+theme(axis.line = element_blank(),
                   legend.position = "bottom")

mission.3<-ggplot(data=core)+
  geom_point(aes(x = Enviro, y = Stated_Environment, shape = leg, color = HubID), size = 5,
             position = position_jitter(h=0.05, w=0.05), alpha = 0.5)+
  expand_limits(x=c(0,3), y=c(0,3))+
  geom_abline(intercept = 0, slope = 1,
              linetype="dashed", size = 1) +
  scale_colour_discrete(guide = "none") +
  labs(title="Stated vs. Implied Missions", x="Implied Environmental", y="Stated Environmental",
       shape="Legal Status")+
<<<<<<< HEAD
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
  geom_bar(stat = "count") +
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


=======
  theme_bw()+theme(axis.line = element_blank(),
                   legend.position = "bottom")
>>>>>>> 50272f8abdf864d165546e04cdb4d7fd74dcea07
