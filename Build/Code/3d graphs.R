#This pulls in the Analysis data and creates visualization with the larger sample size
#Created: December 12, 2022


rm(list=ls())

suppressPackageStartupMessages(library(tidyverse))
library(readxl)
library(scatterplot3d)
library(rgl)
library(fmsb)


#Read in Data####


core<-read_excel("./Data/master.xlsx", sheet = "export")


#Using legal status an product mix

pdata <- core %>%
  select(HubID, LegStat, Small, Medium, Large, Product_Mix) %>%
  mutate(leg_color = case_when(LegStat == "corp" ~ "red",
                               LegStat == "nonprofit" ~ "blue",
                               LegStat == "coop" ~ "green"),
         sum = Small+Medium+Large,
         pm_size = case_when(Product_Mix == "specialized" ~ 0.01,
                             Product_Mix == "narrow" ~ .0150,
                             Product_Mix == "variety" ~ .0250,
                             Product_Mix == "full-spectrum" ~ .0350)) %>%
  filter(sum!=0)

rgl.clear()
plot3d(pdata$Small, pdata$Medium, pdata$Large, col=pdata$leg_color, box=FALSE, axis.scales = FALSE, 
        xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1), type="s",
       radius = pdata$pm_size)

grid3d(c("x","y","z"), at = NULL, col = "gray", lwd = 1, lty = 1, n = 5)



scatterplot3d(pdata$Small, pdata$Medium, pdata$Large, color = pdata$leg_color, pch=16)



pdata <- core %>%
  select(HubID, LegStat, Global_Economic, Global_Social, Global_Environment, Product_Mix) %>%
  mutate(leg_color = case_when(LegStat == "corp" ~ "red",
                               LegStat == "nonprofit" ~ "blue",
                               LegStat == "coop" ~ "green"),
         pm_size = case_when(Product_Mix == "specialized" ~ 0.01,
                             Product_Mix == "narrow" ~ .0150,
                             Product_Mix == "variety" ~ .0250,
                             Product_Mix == "full-spectrum" ~ .0350)) %>%
  filter(!is.na(pm_size))

rgl.clear()
plot3d(pdata$Global_Economic, pdata$Global_Social, pdata$Global_Environment, col=pdata$leg_color, box=FALSE, axis.scales = FALSE, 
       xlim = c(0, 3.25), ylim = c(0, 3.25), zlim = c(0, 3.25), type="s",
       radius = pdata$pm_size)

