
clock <- function(x, y, col, shp, sz, df){
  #X indicates the x-axis variable data frame
  #y indicates the y-axis variable
  #col indicates the color variable
  #shp indicates the shape variable
  #sz indicates the size variable
  #df indicates the dataframe

#Create Variables for Base Graph####
#This creates the points for the circles based on the number of food hubs in the data. Currently this is fixed

  i<-(length(x)*100)-50
  j<-(length(y)*100)-50
  
  circles<-data.frame(
            xcor = rep(seq(50,i,100), length(y)),
            ycor = rep(seq(50,j,100), each=length(x)),
            rad = 40)
  rm(i, j)

#This creates the divisions for the y-axis based on the number of categories in the variable on the y-axis
  i<-length(x)*100
  j<-length(y)*100

  hlines<-data.frame(
            x = rep(0, length(y)+1), #start at zero and the last element is the number of categories plus one.
            y = seq(0,j,100),
            xend = rep(i,length(y)+1), 
            yend = seq(0,j,100))

#This creates the divisions for the x-axis based on the number of categories in the variable on the x-axis.
  vlines<-data.frame(
            x = seq(0,i,100),
            y = rep(0,length(x)+1),
            xend = seq(0,i,100),
            yend = rep(j,length(x)+1))
  
  rm(i, j)
#####
  
#Creates the data
  vars<-c(col, shp, sz) 
  
  temp<-df %>%
    select(all_of(vars), a) %>%
    bind_cols(x, y)
  
  
main <- temp %>%
  mutate(xc = 50+(40*(cos(a*(pi/180)))),
         yc = 50+(40*(sin(a*(pi/180)))))


#Define Color Palette

base<-ggplot(main) +
  scale_x_continuous(breaks = seq(50, (length(x)*100)-50, 100), 
                     labels=sub("_", " ", names(x)), limits=c(0,length(x)*100)) +
  scale_y_continuous(breaks = seq(50, (length(y)*100)-50, 100), 
                     labels = sub("_", " ", names(y)), 
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
  xlab(sub("_", " ", deparse(substitute(x))))+
  ylab(sub("_", " ", deparse(substitute(y))))+
  guides(shape = guide_legend(title=shp),
         size = "none")+
  scale_colour_manual(
    breaks = levels(factor(pull(main[,col]))),
    values = colorRampPalette(c("red", "blue"))(length(levels(factor(pull(main[,col]))))),
    name = gsub("_"," ", col)) +
  labs(caption = paste0("Size based on ", sz))

    for(j in seq(1, length(x))){
    
      r<-names(x)[j]
      
      for(i in seq(1,length(y))){
        s<-names(y)[i]
        k<-(i-1)*100
        temp<-main %>%
          subset(eval(parse(text=r)) == 1 & eval(parse(text=s)) == 1) %>%
          mutate(yc = yc+k)
        
        ifelse(i==1, temp2<-temp, temp2<-rbind(temp2, temp))
      }
      m<-(j-1)*100
      temp3<-temp2 %>%
        mutate(xc = xc+m)
      
      ifelse(j==1, out <- temp3, out<-rbind(out, temp3))
      
    }


base <- base+
      geom_point(data=out, aes(x = xc, y = yc, colour = factor(pull(out[,col])),
                               shape = factor(pull(out[,shp])),
                               size = factor(pull(out[,sz]))))

return(base)

}




