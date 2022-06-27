jradar<-function(plot.data){

    CalculateAxisPath<-function (var.names, min, max) 
    {
      n.vars <- length(var.names)
      angles <- seq(from = 0, to = 2 * pi, by = (2 * pi)/n.vars)
      min.x <- min * sin(angles)
      min.y <- min * cos(angles)
      max.x <- max * sin(angles)
      max.y <- max * cos(angles)
      axisData <- NULL
      for (i in 1:n.vars) {
        a <- c(i, min.x[i], min.y[i])
        b <- c(i, max.x[i], max.y[i])
        axisData <- rbind(axisData, a, b)
      }
      colnames(axisData) <- c("axis.no", "x", "y")
      rownames(axisData) <- seq(1:nrow(axisData))
      as.data.frame(axisData)
    }

      CalculateGroupPath<-function (df) 
      {
        path <- df[, 1]
        angles <- seq(from = 0, to = 2 * pi, by = (2 * pi)/(ncol(df) - 
                                                              1))
        graphData <- data.frame(seg = "", x = 0, y = 0)
        graphData <- graphData[-1, ]
        for (i in levels(path)) {
          pathData <- subset(df, df[, 1] == i)
          for (j in c(2:ncol(df))) {
            graphData <- rbind(graphData, 
                               data.frame(group = i,
                                          x = pathData[, j] * sin(angles[j - 1]), 
                                          y = pathData[, j] * cos(angles[j - 1])))
          }
          graphData <- rbind(graphData, 
                             data.frame(group = i, 
                                        x = pathData[,2] * sin(angles[1]), 
                                        y = pathData[, 2] * cos(angles[1])))
        }
        colnames(graphData)[1] <- colnames(df)[1]
        graphData$group <- factor(graphData$group, levels = levels(df[,1]))
        graphData
      }
      
      funcCircleCoords<-function (center = c(0, 0), r = 1, npoints = 100) 
      {
        tt <- seq(0, 2 * pi, length.out = npoints)
        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)
        return(data.frame(x = xx, y = yy))
      }

#Set Parameter####

base.size = 15
font.radar = "sans" 
#values.radar = c("0%""50%", "100%") 
axis.labels = colnames(plot.data)[-1] 
grid.min = 0 
grid.mid1 = 1
grid.mid2 = 2
grid.max = 3 
centre.y = grid.min - ((1/9) * (grid.max - grid.min)) 
plot.extent.x.sf = 1 
plot.extent.y.sf = 1.2 
x.centre.range = 0.02 * (grid.max - centre.y) 
label.centre.y = FALSE 
grid.line.width = 0.5 
gridline.min.linetype = "solid" 
gridline.mid1.linetype = "longdash" 
gridline.mid2.linetype = "longdash" 
gridline.max.linetype = "solid" 
gridline.min.colour = "grey" 
gridline.mid1.colour = "#007A87" 
gridline.mid2.colour = "#007A87" 
gridline.max.colour = "black" 
grid.label.size = 6 
gridline.label.offset = -0.1 * (grid.max - centre.y) 
label.gridline.min = TRUE
label.gridline.mid = TRUE
label.gridline.max = TRUE
axis.label.offset = 1.15
axis.label.size = 5
axis.line.colour = "grey" 
group.line.width = 1.5 
group.point.size = 6 
group.colours = NULL
background.circle.colour = "#D7D6D1" 
background.circle.transparency = 0.2
plot.legend = if (nrow(plot.data) > 1) TRUE else FALSE
legend.title = ""
plot.title = ""
    legend.text.size = 14
    legend.position = "left"
    fill = FALSE
    fill.alpha = 0.5
    
    
#Graph Setup
    
    plot.data <- as.data.frame(plot.data)
    if (!is.factor(plot.data[, 1])) {
      plot.data[, 1] <- as.factor(as.character(plot.data[,1]))}
    
      names(plot.data)[1] <- "group"
      var.names <- colnames(plot.data)[-1]
      plot.extent.x <- (grid.max + abs(centre.y)) * plot.extent.x.sf
      plot.extent.y <- (grid.max + abs(centre.y)) * plot.extent.y.sf
      plot.data.offset <- plot.data
      plot.data.offset[, 2:ncol(plot.data)] <- plot.data[, 2:ncol(plot.data)] + abs(centre.y)
      
      #Group
      group <- NULL
      group$path <- CalculateGroupPath(plot.data.offset)
      
      #Axis
      axis <- NULL
      axis$path <- CalculateAxisPath(var.names, grid.min + abs(centre.y), 
                                     grid.max + abs(centre.y))
      axis$label <- data.frame(text = axis.labels, x = NA, y = NA)
      n.vars <- length(var.names)
      angles <- seq(from = 0, to = 2 * pi, by = (2 * pi)/n.vars)
      axis$label$x <- sapply(1:n.vars, function(i, x) {
        ((grid.max + abs(centre.y)) * axis.label.offset) * sin(angles[i])
      })
      axis$label$y <- sapply(1:n.vars, function(i, x) {
        ((grid.max + abs(centre.y)) * axis.label.offset) * cos(angles[i])
      })
      
      #Gridlines
      gridline <- NULL
      gridline$min$path <- funcCircleCoords(c(0, 0), grid.min + abs(centre.y), npoints = 360)
      gridline$mid1$path <- funcCircleCoords(c(0, 0), grid.mid1 + abs(centre.y), npoints = 360)
      gridline$mid2$path <- funcCircleCoords(c(0, 0), grid.mid2 + abs(centre.y), npoints = 360)
      gridline$max$path <- funcCircleCoords(c(0, 0), grid.max + abs(centre.y), npoints = 360)
      gridline$min$label <- data.frame(x = gridline.label.offset, 
                                       y = grid.min + abs(centre.y), 
                                       text = as.character(grid.min))
      gridline$mid1$label <- data.frame(x = gridline.label.offset, 
                                        y = grid.mid1 + abs(centre.y),
                                        text = as.character(grid.mid1))
      gridline$mid2$label <- data.frame(x = gridline.label.offset, 
                                        y = grid.mid2 + abs(centre.y), 
                                        text = as.character(grid.mid2))
      gridline$max$label <- data.frame(x = gridline.label.offset, 
                                       y = grid.max + abs(centre.y), 
                                       text = as.character(grid.max))

      #Define Theme to clear background
      theme_clear <- theme_bw(base_size = base.size) + theme(axis.text.y = element_blank(), 
                                                             axis.text.x = element_blank(), axis.ticks = element_blank(), 
                                                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                             panel.border = element_blank(), legend.key = element_rect(linetype = "blank"))
      
          
#Create Graph
    
      #Base Graph and Group Labels
      
      base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() + 
        geom_text(data = subset(axis$label, axis$label$x < (-x.centre.range)), 
                  aes(x = x, y = y, label = text), 
                  size = axis.label.size, 
                  hjust = 1, family = font.radar) + 
        scale_x_continuous(limits = c(-1.5 * plot.extent.x, 1.5 * plot.extent.x)) + 
        scale_y_continuous(limits = c(-plot.extent.y, plot.extent.y))+
        labs(colour = legend.title, size = legend.text.size)+
        geom_text(aes(x = x, y = y, label = as.character(grid.min)), 
                  data = gridline$min$label, size = grid.label.size * 
                    0.8, hjust = 1, family = font.radar)+
        geom_text(aes(x = x, y = y, label = as.character(grid.mid1)), 
                  data = gridline$mid1$label, size = grid.label.size * 
                    0.8, hjust = 1, family = font.radar)+
        geom_text(aes(x = x, y = y, label = as.character(grid.mid2)), 
                data = gridline$mid2$label, size = grid.label.size * 
                  0.8, hjust = 1, family = font.radar)+
        geom_text(aes(x = x, y = y, label = as.character(grid.max)), 
                  data = gridline$max$label, size = grid.label.size * 
                    0.8, hjust = 1, family = font.radar)
      
      base <- base + geom_text(data = subset(axis$label, abs(axis$label$x) <= x.centre.range), 
                               aes(x = x, y = y, label = text), 
                               size = axis.label.size, 
                               hjust = 0.5, family = font.radar)
      
      base <- base + geom_text(data = subset(axis$label, axis$label$x > x.centre.range), 
                               aes(x = x, y = y, label = text), 
                               size = axis.label.size, 
                               hjust = 0, family = font.radar)
      
      base <- base + theme_clear
      
      #Grid Lines
      base <- base + geom_path(data = gridline$min$path, aes(x = x, 
                                                             y = y), 
                               lty = gridline.min.linetype, 
                               colour = gridline.min.colour, 
                               size = grid.line.width)
      base <- base + geom_path(data = gridline$mid1$path, aes(x = x, 
                                                              y = y), 
                               lty = gridline.mid1.linetype, 
                               colour = gridline.mid1.colour, 
                               size = grid.line.width)
      
      base <- base + geom_path(data = gridline$mid2$path, aes(x = x, 
                                                              y = y), 
                               lty = gridline.mid2.linetype, 
                               colour = gridline.mid2.colour, 
                               size = grid.line.width)
      
      base <- base + geom_path(data = gridline$max$path, aes(x = x, 
                                                             y = y), 
                               lty = gridline.max.linetype, 
                               colour = gridline.max.colour, 
                               size = grid.line.width)
    
  #Plot Data
      base <- base + geom_path(data = axis$path, 
                               aes(x = x, y = y, group = axis.no), 
                               colour = axis.line.colour)+
                     geom_path(data = group$path, 
                               aes(x = x, y = y, group = group, colour = group), 
                               size = group.line.width) +
                     geom_point(data = group$path, 
                                aes(x = x, y = y, group = group, colour = group), 
                                size = group.point.size)
return(base)
}