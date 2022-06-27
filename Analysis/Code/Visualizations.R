#This pulls in the Analysis data and creates visualization with the larger sample size
#Created: June 13, 2022


rm(list=ls())

library(ggplot2)
library(ggradar)
  suppressPackageStartupMessages(library(tidyverse))
library(scales)
library(readxl)
library(cowplot)


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


labs<-c("Economic", "Social","Environmental","Community")

f.core<-rbind(f.coop, f.fp, f.nfp)

f.core <- f.core %>%
  mutate(across(2:5, ~ .x / 3))


r = √ ( x2 + y2 )
θ = tan-1 ( y / x )



ggradar(f.core, 
        rescale(FALSE),
        grid.max = 3,
        ylim(0,3))

ggradar(f.core,
        
        plot.legend=TRUE, 
        legend.title = "Type",
        legend.position = "left",
        axis.label.size = 4, 
        grid.label.size = 4, 
        legend.text.size = 10,
        axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=14))+
  labs(title="")

ggradar(f.coop,
              values.radar = c("0","1", "2", "3"),
        grid.min = 0,
        grid.mid = 2,
        grid.max = 4,
              plot.legend=FALSE, 
              axis.label.size = 4, 
              grid.label.size = 4, 
              legend.text.size = 10,
              axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=14))+
  labs(title="How would you rate the direct relevance of the following to the mission of your food hub?
LLC's")

ggradar(f.fp,
                values.radar = c("0","1", "2", "3"),
        grid.min = 0,
        grid.mid = 2,
        grid.max = 4,
                plot.legend=FALSE, 
                axis.label.size = 4, 
                grid.label.size = 4, 
                legend.text.size = 10,
                axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=14))+
  labs(title="")

ggradar(f.nfp,
                values.radar = c("0", "1", "2", "3"),
                grid.min = 0,
                grid.mid = 2,
                grid.max = 4,
                plot.legend=FALSE,
                axis.label.size = 4, 
                grid.label.size = 4, 
                legend.text.size = 10,
                axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=14))+
  labs(title="")

