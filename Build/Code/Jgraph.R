#This pulls in the Analysis data and creates visualization with the larger sample size
#Specifically this creates the custom designed visualization
#Created: December 13, 2022


rm(list=ls())

library(ggforce)
library(tidyverse)
library(readxl)
library(graphics)

core<-read_excel("./Data/master.xlsx", sheet = "export")


gdata<-core %>%
  select(HubID, a, LegStat, Business_Model, Product_Mix) %>%
  mutate(x = 50+(40*(cos(a*(pi/180)))),
         y = 50+(40*(sin(a*(pi/180)))),
         Product_Mix = as.factor(Product_Mix))






circles<-data.frame(
  x = rep(seq(50,750,100), 5),
  y = rep(seq(50,450,100), each=8),
  r = 40
)

hlines<-data.frame(
  x = rep(0, 6),
  y = seq(0,500,100),
  xend = rep(800,6), 
  yend = seq(0,500,100)
)

vlines<-data.frame(
  x = seq(0,800,100),
  y = rep(0,9),
  xend = seq(0,800,100),
  yend = rep(500,9)
)


ggplot(gdata) +
  scale_x_continuous(breaks = c(50, 150, 250, 350, 450, 550, 650, 750), 
                     labels=c("Warehous","Office", "Trucks", "Online Systems", "Retail",
                              "Shared Kitchen", "Processing", "None"), limits=c(0,800)) +
  scale_y_continuous(breaks = c(50, 150, 250, 350, 450), 
                     labels = c("Direct to Consumer", "Mostly DtC", "Hybrid", "Mostly Wholesale", "Wholesale"), 
                     limits=c(0, 500))+
  scale_colour_manual(
    values = c("full-spectrum" = "red","narrow" = "blue","variety" = "green", "unknown" = "gray", "specialized" = "navy"),
    name = "Product Mix")+

  geom_point(aes(x=x, y=y, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Warehouse==1 & gdata$Business_Model == "DtC"),]) +
  geom_point(aes(x=x, y=y+100, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Warehouse==1 & gdata$Business_Model == "Mostly DtC"),])+
  geom_point(aes(x=x, y=y+200, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Warehouse==1 & gdata$Business_Model == "Hybrid"),])+
  geom_point(aes(x=x, y=y+300, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Warehouse==1 & gdata$Business_Model == "Mostly Wholesale"),])+
  geom_point(aes(x=x, y=y+400, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Warehouse==1 & gdata$Business_Model == "Wholesale"),])+
  
  geom_point(aes(x=x+100, y=y, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Office==1 & gdata$Business_Model == "DtC"),]) +
  geom_point(aes(x=x+100, y=y+100, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Office==1 & gdata$Business_Model == "Mostly DtC"),])+
  geom_point(aes(x=x+100, y=y+200, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Office==1 & gdata$Business_Model == "Hybrid"),])+
  geom_point(aes(x=x+100, y=y+300, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Office==1 & gdata$Business_Model == "Mostly Wholesale"),])+
  geom_point(aes(x=x+100, y=y+400, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Office==1 & gdata$Business_Model == "Wholesale"),])+
  
  geom_point(aes(x=x+200, y=y, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Trucks==1 & gdata$Business_Model == "DtC"),]) +
  geom_point(aes(x=x+200, y=y+100, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Trucks==1 & gdata$Business_Model == "Mostly DtC"),])+
  geom_point(aes(x=x+200, y=y+200, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Trucks==1 & gdata$Business_Model == "Hybrid"),])+
  geom_point(aes(x=x+200, y=y+300, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Trucks==1 & gdata$Business_Model == "Mostly Wholesale"),])+
  geom_point(aes(x=x+200, y=y+400, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Trucks==1 & gdata$Business_Model == "Wholesale"),])+
  
  geom_point(aes(x=x+300, y=y, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Online_System==1 & gdata$Business_Model == "DtC"),]) +
  geom_point(aes(x=x+300, y=y+100, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Online_System==1 & gdata$Business_Model == "Mostly DtC"),])+
  geom_point(aes(x=x+300, y=y+200, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Online_System==1 & gdata$Business_Model == "Hybrid"),])+
  geom_point(aes(x=x+300, y=y+300, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Online_System==1 & gdata$Business_Model == "Mostly Wholesale"),])+
  geom_point(aes(x=x+300, y=y+400, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Online_System==1 & gdata$Business_Model == "Wholesale"),])+
  
  geom_point(aes(x=x+400, y=y, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Retail==1 & gdata$Business_Model == "DtC"),]) +
  geom_point(aes(x=x+400, y=y+100, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Retail==1 & gdata$Business_Model == "Mostly DtC"),])+
  geom_point(aes(x=x+400, y=y+200, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Retail==1 & gdata$Business_Model == "Hybrid"),])+
  geom_point(aes(x=x+400, y=y+300, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Retail==1 & gdata$Business_Model == "Mostly Wholesale"),])+
  geom_point(aes(x=x+400, y=y+400, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Retail==1 & gdata$Business_Model == "Wholesale"),])+
  
  geom_point(aes(x=x+500, y=y, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Shared_Kitchen==1 & gdata$Business_Model == "DtC"),]) +
  geom_point(aes(x=x+500, y=y+100, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Shared_Kitchen==1 & gdata$Business_Model == "Mostly DtC"),])+
  geom_point(aes(x=x+500, y=y+200, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Shared_Kitchen==1 & gdata$Business_Model == "Hybrid"),])+
  geom_point(aes(x=x+500, y=y+300, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Shared_Kitchen==1 & gdata$Business_Model == "Mostly Wholesale"),])+
  geom_point(aes(x=x+500, y=y+400, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Shared_Kitchen==1 & gdata$Business_Model == "Wholesale"),])+
  
  geom_point(aes(x=x+600, y=y, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Processing==1 & gdata$Business_Model == "DtC"),]) +
  geom_point(aes(x=x+600, y=y+100, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Processing==1 & gdata$Business_Model == "Mostly DtC"),])+
  geom_point(aes(x=x+600, y=y+200, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Processing==1 & gdata$Business_Model == "Hybrid"),])+
  geom_point(aes(x=x+600, y=y+300, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Processing==1 & gdata$Business_Model == "Mostly Wholesale"),])+
  geom_point(aes(x=x+600, y=y+400, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$Processing==1 & gdata$Business_Model == "Wholesale"),])+
  
  geom_point(aes(x=x+700, y=y, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$None==1 & gdata$Business_Model == "DtC"),]) +
  geom_point(aes(x=x+700, y=y+100, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$None==1 & gdata$Business_Model == "Mostly DtC"),])+
  geom_point(aes(x=x+700, y=y+200, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$None==1 & gdata$Business_Model == "Hybrid"),])+
  geom_point(aes(x=x+700, y=y+300, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$None==1 & gdata$Business_Model == "Mostly Wholesale"),])+
  geom_point(aes(x=x+700, y=y+400, shape=LegStat, colour=Product_Mix), size=4, 
             data=gdata[which(gdata$None==1 & gdata$Business_Model == "Wholesale"),])+
  #Visuals
  geom_segment(aes(x=x, xend=xend, y=y, yend=yend), data=hlines)+
  geom_segment(aes(x=x, xend=xend, y=y, yend=yend), data=vlines)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.y = element_text(face="bold",size=9, angle=0),
        axis.text.x = element_text(face="bold",size=9, angle=90)) +
  geom_circle(aes(x0 = x, y0=y, r = r), colour = "gray", linetype="dotted", data=circles,  inherit.aes=FALSE) +
  xlab("Infrastructure")+
  ylab("Business Model")+
  guides(shape = guide_legend(title="Legal Status"))

##################################################

gdata<-core %>%
  select(HubID, Hub_Name, starts_with("Local"), starts_with("Global"), LegStat, Product_Mix, Business_Model) %>%
  mutate(ID = row_number(),
         Global_Social = as.numeric(Global_Social),
         Local_Social = as.numeric(Local_Social),
         Global_Economic = as.numeric(Global_Economic),
         Local_Economic = as.numeric(Local_Economic),
         Global_Environment = as.numeric(Global_Environment),
         Local_Environmental = as.numeric(Local_Environmental),
         Product_Mix = factor(Product_Mix, levels=c("unknown", "specialized", "narrow", "variety", 
                                                    "full-spectrum")),
         Business_Model = factor(Business_Model, levels=c("DtC", "Mostly DtC", "Hybrid","Mostly Wholesale","Wholesale")))

ggplot(gdata) +

  geom_point(aes(x = Global_Social, y=HubID, colour=Business_Model, shape = LegStat, size = Product_Mix)) +
  geom_segment(aes(x = Local_Social, xend = Global_Social, y=ID, yend=ID)) +
  geom_point(aes(x = Local_Social, y=HubID, colour=Business_Model, shape = LegStat, size=Product_Mix),alpha = 0.2) +
  scale_x_continuous(limits=c(0,3)) +
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.y = element_text(face="bold",size=9, angle=0),
        axis.text.x = element_text(face="bold",size=9, angle=0)) +
  xlab("Importance")+
  ylab("Food Hub")+
  guides(shape = guide_legend(title="Legal Status"),
         colour = guide_legend(title = "Business Model"),
         size = guide_legend(title="Product Mix")) +
  labs(title = "Mission: Social", caption = "Dark = Global; Light = Local(average of individual missions)")

ggplot(gdata) +
  geom_point(aes(x = Global_Economic, y=HubID, colour=Business_Model, size=Product_Mix, shape = LegStat)) +
  geom_segment(aes(x = Local_Economic, xend = Global_Economic, y=ID, yend=ID)) +
  geom_point(aes(x = Local_Economic, y=HubID, colour=Business_Model, size=Product_Mix, shape = LegStat), alpha = 0.2) +
  scale_x_continuous(limits=c(0,3)) +
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.y = element_text(face="bold",size=9, angle=0),
        axis.text.x = element_text(face="bold",size=9, angle=0)) +
  xlab("Importance")+
  ylab("Food Hub")+
  guides(shape = guide_legend(title="Legal Status"),
         colour = guide_legend(title = "Business Model"),
         size = guide_legend(title="Product Mix")) +
  labs(title = "Mission: Economic", caption = "Dark = Global; Light = Local(average of individual missions)")

ggplot(gdata) +
  geom_point(aes(x = Global_Environment, y=HubID, colour=Business_Model, size=Product_Mix, shape = LegStat)) +
  geom_segment(aes(x = Local_Environmental, xend = Global_Environment, y=ID, yend=ID)) +
  geom_point(aes(x = Local_Environmental, y=HubID, colour=Business_Model, size=Product_Mix, shape = LegStat), 
             alpha = 0.2) +
  scale_x_continuous(limits=c(0,3)) +
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.y = element_text(face="bold",size=9, angle=0),
        axis.text.x = element_text(face="bold",size=9, angle=0)) +
  xlab("Importance")+
  ylab("Food Hub")+
  guides(shape = guide_legend(title="Legal Status"),
         colour = guide_legend(title = "Business Model"),
         size = guide_legend(title="Product Mix")) +
  labs(title = "Mission: Environment", caption = "Dark = Global; Light = Local(average of individual missions)")
