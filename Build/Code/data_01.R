# Data input and Visualization
#Updated Sept 14, 2022

rm(list=ls())

library(tidyverse)


#Pull in Data from Master Excel File
#NOTE: Copy Master FH Data.xlsx from OneDrive to Project Data Folder as master.xlsx

legal <- read_excel("./Data/master.xlsx", sheet = "Legal Classification Employees")
  names(legal) <- c("HubID", "HubName", "LegalSt", "Employees")
  legal <- legal %>%
              filter(!is.na(LegalSt)) %>%
              mutate(LegalSt = str_to_sentence(LegalSt))

employ <- legal[!is.na(legal$Employees),]

  legal$Employees<-NULL
  
decis <- read_excel("./Data/master.xlsx", sheet = "Decision Making")

mission <- read_excel("./Data/master.xlsx", sheet = "Mission Statements")
  miss_score <- mission %>%
    select(HubID, ends_with("Mean"))  %>%
    filter(!is.na(HubID))%>%
    pivot_longer(!HubID, names_to = "cat", values_to = "score") %>%
    group_by(HubID)%>%
    arrange(desc(score), .by_group = TRUE) %>%
    mutate(max = cat[which.max(score)],
           min = cat[which.min(score)],
           score2 = case_when(cat==max ~3,
                              cat==min ~1,
                              TRUE ~2),
           mid = cat[which(score2==2)],
           max = str_to_sentence(gsub(" Mean", "", max)),
           mid = str_to_sentence(gsub(" Mean", "", mid)),
           min = str_to_sentence(gsub(" Mean", "", min)))  %>%
    select(HubID, max, mid, min)%>%
    distinct()
  
mission_st <- read_excel("./Data/master.xlsx", sheet = "Stated Mission Focus")
  mission_st <- mission_st %>%
    select(HubID, starts_with("st_")) %>%
    pivot_longer(!HubID, names_to = "cat", values_to = "score") %>%
    group_by(HubID) %>%
    mutate(max_st = cat[which.max(score)],
           min_st = cat[which.min(score)],
           score2 = case_when(cat==max_st ~3,
                              cat==min_st ~1,
                              TRUE ~2),
           mid_st = cat[which(score2==2)],
           max_st = str_to_sentence(gsub("St_", "", max_st)),
           mid_st = str_to_sentence(gsub("St_", "", mid_st)),
           min_st = str_to_sentence(gsub("St_", "", min_st))) %>%
    select(HubID, max_st, mid_st, min_st) %>%
    distinct()
  

infrast <- read_excel("./Data/master.xlsx", sheet = "Infrastructure")
  infrast$Infrastructure <- NULL
  infrast$'Hub Name'<-NULL
  infrast <- infrast %>%
              pivot_longer(!HubID, names_to = "Infrastructure", values_to = "count")
  

prodmix <- read_excel("./Data/master.xlsx", sheet = "Product Mix")

farmscal <- read_excel("./Data/master.xlsx", sheet = "Farm Scale")
  names(farmscal) <- c("HubID", "HubName", "FarmScale", "Small", "Medium", "Large")
  

sales <- read_excel("./Data/master.xlsx", sheet = "Sale Points")

socgoals <- read_excel("./Data/master.xlsx", sheet = "Social Goals")

envpro <- read_excel("./Data/master.xlsx", sheet = "Environmental Programs")


##Visualizations#####

temp<-inner_join(legal, infrast, by="HubID")

temp <- temp %>%
  filter(count != 0)

temp<-inner_join(temp, miss_score, by="HubID")

temp<-inner_join(temp, mission_st, by="HubID")

plot<-ggplot(temp, aes(y=Infrastructure)) +
  geom_bar(aes(fill = LegalSt), position = position_stack(reverse = TRUE)) +
  labs(title = "Calcuated Mission vs. Stated Mission Score",
       subtitle = "Primary Focus Category",
       x = "Number of Hubs Providing")+
  theme(legend.position = "top") +
  facet_grid(max ~ max_st)

plot2<-ggplot(temp, aes(y=Infrastructure)) +
  geom_bar(aes(fill = LegalSt), position = position_stack(reverse = TRUE)) +
  labs(title = "Calcuated Mission vs. Stated Mission Score",
       subtitle = "Secondary Focus Category",
       x = "Number of Hubs Providing")+
  theme(legend.position = "top") +
  facet_grid(mid ~ mid_st)

plot3<-ggplot(temp, aes(y=Infrastructure)) +
  geom_bar(aes(fill = LegalSt), position = position_stack(reverse = TRUE)) +
  labs(title = "Calcuated Mission vs. Stated Mission Score",
       subtitle = "Tertiary Focus Category",
       x = "Number of Hubs Providing")+
  theme(legend.position = "top") +
  facet_grid(min ~ min_st)
