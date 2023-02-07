
gdata <- core %>%
  select(HubID, a, LegStat, Scale_Color, Business_Model,  Warehouse, Office,
         Trucks, Online_System, Retail, Processing, Shared_Kitchen, Product_Mix) %>%
  mutate(x = 50+(40*(cos(a*(pi/180)))),
         y = 50+(40*(sin(a*(pi/180)))),
         Product_Mix = as.factor(Product_Mix),
         sum = rowSums(across(Warehouse:Shared_Kitchen)),
         None = case_when(sum == 0 ~ 1,
                          TRUE ~ 0))

clock <- function(x, y, a, b, c){
  #X indicates the x-axis variable data frame
  #y indicates the y-axis variable
  #a indicates the color variable
  #b indicates the shape variable
  #c indicates the size variable

#This creates the points for the circles based on the number of food hubs in the data. Currently this is fixed

<<<<<<< HEAD
  i<-(length(x)*100)-50
  j<-(length(y)*100)-50
  
  circles<-data.frame(
  xcor = rep(seq(50,i,100), length(y)),
  ycor = rep(seq(50,j,100), each=length(x)),
  rad = 40
)
  
rm(i, j)

#This creates the divisions for the y-axis based on the number of categories in the variable on the y-axis
  i<-length(x)*100
  j<-length(y)*100
  
  hlines<-data.frame(
  x = rep(0, length(y)+1), #start at zero and the last element is the number of categories plus one.
  y = seq(0,j,100),
  xend = rep(i,length(y)+1), 
  yend = seq(0,j,100)
=======
i<-length(x)
  
  circles<-data.frame(
  xcor = rep(seq(50,750,100), length(y)),
  ycor = rep(seq(50,450,100), length(x)),
  rad = 40
)

#This creates the divisions for the y-axis based on the number of categories in the variable on the y-axis
hlines<-data.frame(
  x = rep(0, length(levels(y))), #start at zero and the last element is the number of categories plus one.
  y = seq(0,500,100),
  xend = rep(800,6), 
  yend = seq(0,500,100)
>>>>>>> 07fa670f187bcbb84e395e906f2a3cdb8c6b1c5a
)

#This creates the divisions for the x-axis based on the number of categories in the variable on the x-axis.
vlines<-data.frame(
<<<<<<< HEAD
  x = seq(0,i,100),
  y = rep(0,length(x)+1),
  xend = seq(0,i,100),
  yend = rep(j,length(x)+1)
)

#Plots the actual points on the clock

d1<-gdata %>%
  select(x, y, Product_Mix, LegStat)

d2<-cbind(d1, x, y)

base<-ggplot(d1) +
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
  guides(shape = guide_legend(title="Legal Status"))+
  scale_colour_manual(
    breaks = levels(d2$Product_Mix),
    values = colorRampPalette(c("red", "blue"),length(levels(d2$Product_Mix))),
    name = "Product Mix") +
  geom_point(aes(x=x, y=y, colour=Product_Mix), size=4,
             data=d2[which(d2$Warehouse==1 & d2$DtC == 1),]) 


=======
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
>>>>>>> 07fa670f187bcbb84e395e906f2a3cdb8c6b1c5a
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
<<<<<<< HEAD
  
  ggplot()+
=======
>>>>>>> 07fa670f187bcbb84e395e906f2a3cdb8c6b1c5a
  geom_segment(aes(x=x, xend=xend, y=y, yend=yend), data=hlines)+
  geom_segment(aes(x=x, xend=xend, y=y, yend=yend), data=vlines)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.y = element_text(face="bold",size=9, angle=0),
        axis.text.x = element_text(face="bold",size=9, angle=90)) +
<<<<<<< HEAD
  geom_circle(aes(x0 = xcor, y0=ycor, r = rad), colour = "gray", linetype="dotted", data=circles,  inherit.aes=FALSE) +
  xlab("Infrastructure")+
  ylab("Business Model")+
  guides(shape = guide_legend(title="Legal Status"))
=======
  geom_circle(aes(x0 = x, y0=y, r = r), colour = "gray", linetype="dotted", data=circles,  inherit.aes=FALSE) +
  xlab("Infrastructure")+
  ylab("Business Model")+
  guides(shape = guide_legend(title="Legal Status"))
>>>>>>> 07fa670f187bcbb84e395e906f2a3cdb8c6b1c5a
