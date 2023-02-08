y<- gdata %>%
  mutate(as.data.frame(model.matrix(~ 0 + Business_Model, data=gdata))) %>%
  select(contains("Business_Model")) %>%
  select(-Business_Model)

names(y)<-gsub("Business_Model","",names(y))

x <- gdata %>%
  select(Warehouse, Office, Trucks, Online_System, Retail, Processing, Shared_Kitchen, None)

core<-gdata %>%
  select(HubID, LegStat, Product_Mix,x, y)


ggplot() +
  scale_x_continuous(breaks = seq(50, (length(x)*100)-50, 100), 
                     labels=names(x), limits=c(0,length(x)*100)) +
  scale_y_continuous(breaks = seq(50, (length(y)*100)-50, 100), 
                     labels = names(y), 
                     limits=c(0, length(y)*100))+
  geom_segment(aes(x=x, xend=xend, y=y, yend=yend), data=hlines)+
  geom_segment(aes(x=x, xend=xend, y=y, yend=yend), data=vlines)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.y = element_text(face="bold",size=9, angle=0),
        axis.text.x = element_text(face="bold",size=9, angle=90)) +
  geom_circle(aes(x0 = xcor, y0=ycor, r = rad), colour = "gray", linetype="dotted", data=circles,  inherit.aes=FALSE) +
  xlab("Infrastructure")+
  ylab("Business Model")+
  guides(shape = guide_legend(title="Legal Status"))
