#This pulls in the Response DAta and creates the Data Visualizations based on HUB Type


rm(list=ls())

library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(tidyverse))
library(scales)
library(readxl)
library(cowplot)


#Read in Response Data

hubs<-read_excel("./Build/Input/Responses.xlsx", sheet = "Hubs")

hubs<-hubs %>%
  filter(!is.na(Lat)) %>%
  mutate(lon1=Lon,
         lat1=Lat)

#Splits the hubs dataframe into dfs by type
focus<-hubs %>%
  group_by(Legal, .add=TRUE) %>%
  group_split()


#Creates the Radar Plots for Question 1.1 of the Survey: Mission#####
f.LLC <- focus[[1]] %>%
  select(-Legal) %>%
  select(HUB,Profit:Training2)
f.MC <- focus[[2]] %>%
  select(-Legal) %>%
  select(HUB,Profit:Training2)
f.NP <- focus[[3]] %>%
  select(-Legal) %>%
  select(HUB,Profit:Training2)

labs<-c("Profitability","Environmental Issues","Economic Dev.","Human Health",
        "Financial Viability","Employment Conditions","Racial Equity","Food Access",
        "An. Welfare","Training in Prod.", "Training in Bus.")

ggradar(f.LLC,
              values.radar = c("1", "2", "3"),
              grid.min = 0,
              grid.mid = 1.5,
              grid.max = 3,
              plot.legend=TRUE, 
              legend.title = "Food Hub",
              legend.position = "left",
              axis.label.size = 4, 
              grid.label.size = 4, 
              legend.text.size = 10,
              axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=14))+
  labs(title="How would you rate the direct relevance of the following to the mission of your food hub?
LLC's")

ggradar(f.MC,
                values.radar = c("1", "2", "3"),
                grid.min = 0,
                grid.mid = 1.5,
                grid.max = 3,
                plot.legend=TRUE, 
                legend.title = "Food Hub",
                legend.position = "left",
                axis.label.size = 4, 
                grid.label.size = 4, 
                legend.text.size = 10,
                axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=14))+
  labs(title="How would you rate the direct relevance of the following to the mission of your food hub?
Multishareholder Coop")

ggradar(f.NP,
                values.radar = c("1", "2", "3"),
                grid.min = 0,
                grid.mid = 1.5,
                grid.max = 3,
                plot.legend=TRUE, 
                legend.title = "Food Hub",
                legend.position = "left",
                axis.label.size = 4, 
                grid.label.size = 4, 
                legend.text.size = 10,
                axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=14))+
  labs(title="How would you rate the direct relevance of the following to the mission of your food hub?
Nonprofits")

#Creates Radar Plots for Question 2.1: Sales Points#######
f.LLC <- focus[[1]] %>%
  select(-Legal) %>%
  select(HUB,29:38) 
f.MC <- focus[[2]] %>%
  select(-Legal) %>%
  select(HUB,29:38)
f.NP <- focus[[3]] %>%
  select(-Legal) %>%
  select(HUB,29:38)

labs<-c("Large Store", "Small Store","Direct-to-Consumer","Resturant",
        "Food Pantry","School","College","Hospital","Prison","Other")

ggradar(f.LLC,
                grid.min = 0,
                grid.mid = 50,
                grid.max = 100,
                plot.legend=TRUE, 
                legend.title = "Food Hub",
                legend.position = "left",
                axis.label.size = 4, 
                grid.label.size = 4, 
                legend.text.size = 10,
                axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=14))+
  labs(title="Please estimate the percentage of your overall yearly sales that goes to 
each of the following: LLC's")

ggradar(f.MC,
                grid.min = 0,
                grid.mid = 50,
                grid.max = 100,
                plot.legend=TRUE, 
                legend.title = "Food Hub",
                legend.position = "left",
                axis.label.size = 4, 
                grid.label.size = 4, 
                legend.text.size = 10,
                axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=14))+
  labs(title="Please estimate the percentage of your overall yearly sales that goes to 
each of the following: Multishareholder Coop")

ggradar(f.NP,
                grid.min = 0,
                grid.mid = 50,
                grid.max = 100,
                plot.legend=TRUE, 
                legend.title = "Food Hub",
                legend.position = "left",
                axis.label.size = 4, 
                grid.label.size = 4, 
                legend.text.size = 10,
                axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=14))+
  labs(title="Please estimate the percentage of your overall yearly sales that goes to 
each of the following: Nonprofits")

 
#Creates  Plots for Question 1.2: Goals#####

f.LLC <- focus[[1]] %>%
  select(Legal,PEY:Donate)
f.MC <- focus[[2]] %>%
  select(Legal,PEY:Donate)
f.NP <- focus[[3]] %>%
  select(Legal,PEY:Donate)

temp<-rbind(f.LLC,f.MC,f.NP)
  temp2<-count(temp,Legal)
  temp<-merge(temp,temp2,by="Legal")
  temp$Legal<-paste0(temp$Legal," (",temp$n,")")
  temp$n<-NULL

temp1<-temp%>%
  group_by(Legal)%>%
  summarise_at(vars(PEY:Donate),
               list(n=sum))%>%
  rename_with(~gsub("_n","",.x,fixed=TRUE)) %>%
  pivot_longer(!Legal,names_to="Services", values_to = "Number")

labs=c("Acc. SNAP","Acc. WIC","Donation","Educate Comm.","Educate Garden",
       "Health Screen","Mobile Market","Match SNAP", "Educate Cook",
       "Paid Youth Opp","Farm Shares","Transport")

ggplot(temp1)+
  geom_col(aes(x=Services, y=Number, fill=Legal))+
           labs(x ="Goals",
                title="Has the hub done any of the following in the past 3 years?")+
  scale_x_discrete(labels= labs)+
  labs(fill ="Type of Hub")+
  theme(axis.text.x=element_text(angle = 90))
  
#Creates Plots for Question 2.2: Infrastructure#####

f.LLC <- focus[[1]] %>%
  select(Legal,WareH:Cold)
f.MC <- focus[[2]] %>%
  select(Legal,WareH:Cold)
f.NP <- focus[[3]] %>%
  select(Legal,WareH:Cold)

temp<-rbind(f.LLC,f.MC,f.NP)
    temp2<-count(temp,Legal)
    temp<-merge(temp,temp2,by="Legal")
    temp$Legal<-paste0(temp$Legal," (",temp$n,")")
    temp$n<-NULL
    rm(temp2)

temp1<-temp%>%
  group_by(Legal)%>%
  summarise_at(vars(WareH:Cold),
               list(n=sum))%>%
  rename_with(~gsub("_n","",.x,fixed=TRUE)) %>%
  pivot_longer(!Legal,names_to="Infrastructure", values_to = "Number")

labs=c("Cold Space","Licensed Kitch.","Office Space","Production Facility",
       "Rental Space","Retail Space","Trucks","Warehouse","Internet Ordering")

ggplot(temp1)+
  geom_col(aes(x=Infrastructure, y=Number, fill=Legal))+
  labs(x ="Infrastructure",
       title="Infrastructure: Which of the following does your hub have?")+
  scale_x_discrete(labels= labs)+
  labs(fill ="Type of Hub")+
  theme(axis.text.x=element_text(angle = 90))

#Creates Radar Plots for Revenue Sources#######
f.LLC <- focus[[1]] %>%
  select(-Legal) %>%
  select(HUB,RevSales:RevOth)%>%
  rename_with(~ (gsub("Rev","", .x, fixed = TRUE)))
f.MC <- focus[[2]] %>%
  select(-Legal) %>%
  select(HUB,RevSales:RevOth)%>%
  rename_with(~ (gsub("Rev","", .x, fixed = TRUE)))
f.NP <- focus[[3]] %>%
  select(-Legal) %>%
  select(HUB,RevSales:RevOth)%>%
  rename_with(~ (gsub("Rev","", .x, fixed = TRUE))) 


labs<-c("Sales","Federal Grant","State Grant","Foundation Grant","Donatitions",
        "Loans","Private Investment","Services","Other")

ggradar(f.LLC,
                grid.min = 0,
                grid.mid = 50,
                grid.max = 100,
                plot.legend=TRUE, 
                legend.title = "Food Hub",
                legend.position = "left",
                axis.label.size = 4, 
                grid.label.size = 4, 
                legend.text.size = 10,
                axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=12))+
  labs(title="Approximate percentage of your hub's overall revenue that you 
receive from each source?: LLC")

ggradar(f.MC,
                grid.min = 0,
                grid.mid = 50,
                grid.max = 100,
                plot.legend=TRUE, 
                legend.title = "Food Hub",
                legend.position = "left",
                axis.label.size = 4, 
                grid.label.size = 4, 
                legend.text.size = 10,
                axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=12))+
  labs(title="Approximate percentage of your hub's overall revenue that you 
receive from each source?: Multishareholder Coop")

ggradar(f.NP,
                grid.min = 0,
                grid.mid = 50,
                grid.max = 100,
                plot.legend=TRUE, 
                legend.title = "Food Hub",
                legend.position = "left",
                axis.label.size = 4, 
                grid.label.size = 4, 
                legend.text.size = 10,
                axis.labels = labs) +
  theme(plot.title = element_text(size=14),
        legend.title = element_text(size=12))+
  labs(title="Approximate percentage of your hub's overall revenue that you 
receive from each source?: Nonprofits")
