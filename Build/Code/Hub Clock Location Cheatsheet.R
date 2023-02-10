rm(list=ls())

library(ggforce)
library(tidyverse)
library(readxl)
library(scales)


core<-read_excel("./Data/master.xlsx", sheet = "export")


 main <- core %>%
   mutate(xc = 50+(40*(cos(a*(pi/180)))),
          yc = 50+(40*(sin(a*(pi/180))))) %>%
   select(HubID, Hub_Name, a, xc, yc, LegStat)
         
#Define Color Palette
ggplot(main) +
  theme(panel.border = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      legend.position="bottom") +
  geom_circle(aes(x0 = 50, y0=50, r = 40), colour = "gray", linetype="dotted", inherit.aes=FALSE)+
  xlab(NULL)+
  ylab(NULL)+
  geom_point(aes(x = xc, y=yc, shape=LegStat), size=15) +
  geom_text(aes(x = xc, y=yc, label = HubID), color = "white")+
  geom_text(aes(x = xc, y=yc-8, label = str_wrap(Hub_Name, 10)), color = "red")+
  guides(shape = guide_legend(title="Legal Status", override.aes = list(size = 7)))+
  labs(title = "Food Hub Clock Position Cheatsheet")
        
