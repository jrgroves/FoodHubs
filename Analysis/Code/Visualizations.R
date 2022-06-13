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

load(file="./Build/OUtput/core.RData")

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

ggradar(f.core,
        values.radar = c("0","1", "2", "3"),
        grid.min = 0,
        grid.mid = 2,
        grid.max = 4,
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

