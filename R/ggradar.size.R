ggradar.size <-
function (plot.data,
                          group_col = 1,
                          weight_col = NULL,
                          #
                          # radar limits
                          grid.mid = 0.5,
                          grid.max = 1.0,
                          grid.min = 0,
                          values.radar = c("0%", "50%", "100%"),
                          #
                          #
                          # legend
                          #
                          legend.pos="bottom",
                          #
                          #
                          # labels, points and lines
                          #
                          axis.label.size = 5,
                          group.point.size = 2.5,
                          group.line.width = 2.0,
                          axis.label.offset = 1.15,
                          grid.label.size = 4.5,
                          font.radar="ArialMT",
                          #
                          #
                          # colors
                          #
                          gridline.min.colour = "grey",
                          gridline.mid.colour = "#007A87",
                          gridline.max.colour = "grey",
                          background.circle.colour = "#D7D6D1",
                          axis.line.colour = "grey40",
                          linepoint.alpha = 0.56,
                          background.circle.transparency = 0.2,
                          #
                          #
                          # other parameters
                          #
                          plot.extent.x.sf = 1,
                          plot.extent.y.sf = 1.2,
                          gridline.min.linetype = "longdash",
                          gridline.mid.linetype = "longdash",
                          gridline.max.linetype = "longdash",
                          grid.line.width = 0.5
                          ) 
{
  plot.data[,group_col] <- as.factor(as.character(plot.data[,group_col]))
  names(plot.data)[group_col] <- "group"
  if (!is.null(weight_col)) { names(plot.data)[weight_col] <- "size" }
  #
  var.names <- colnames(plot.data)[-c(group_col, weight_col)]
  #
  #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
  centre.y=grid.min - ((1/9)*(grid.max-grid.min))
  x.centre.range = 0.02*(grid.max-centre.y)
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf
  #
  #
  # ggradar functions
  #
  CalculateGroupPath <- function(data.frm, group_col, weight_col) {
    #Converts variable values into a set of radial x-y coordinates
    #Code adapted from a solution posted by Tony M to
    #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
    #Args:
    #  df: Col 1 -  group ('unique' cluster / group ID of entity)
    #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
    #
    c.idx <- 1:ncol(plot.data)
    c.idx <- c.idx[-c(group_col, weight_col)]
    c.idx <- c(group_col, c.idx)
    df <- data.frm[,c.idx]
    path <- df[,1]
    #
    ##find increment
    angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
    ##create graph data frame
    graphData= data.frame(seg="", x=0,y=0)
    graphData=graphData[-1,]
    
    for(i in levels(path)){
      pathData = subset(df, df[,1]==i)
      for(j in c(2:ncol(df))){
        #pathData[,j]= pathData[,j]
        
        
        graphData=rbind(graphData, data.frame(group=i,
                                              x=pathData[,j]*sin(angles[j-1]),
                                              y=pathData[,j]*cos(angles[j-1])))
      }
      ##complete the path by repeating first pair of coords in the path
      graphData=rbind(graphData, data.frame(group=i,
                                            x=pathData[,2]*sin(angles[1]),
                                            y=pathData[,2]*cos(angles[1])))
    }
    #Make sure that name of first column matches that of input data (in case !="group")
    colnames(graphData)[1] <- colnames(df)[1]
    graphData #data frame returned by function
  }
  CaclulateAxisPath = function(var.names,min,max) {
    #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
    #Args:
    #var.names - list of variables to be plotted on radar plot
    #min - MININUM value required for the plotted axes (same value will be applied to all axes)
    #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
    #var.names <- c("v1","v2","v3","v4","v5")
    n.vars <- length(var.names) # number of vars (axes) required
    #Cacluate required number of angles (in radians)
    angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
    #calculate vectors of min and max x+y coords
    min.x <- min*sin(angles)
    min.y <- min*cos(angles)
    max.x <- max*sin(angles)
    max.y <- max*cos(angles)
    #Combine into a set of uniquely numbered paths (one per variable)
    axisData <- NULL
    for (i in 1:n.vars) {
      a <- c(i,min.x[i],min.y[i])
      b <- c(i,max.x[i],max.y[i])
      axisData <- rbind(axisData,a,b)
    }
    #Add column names + set row names = row no. to allow conversion into a data frame
    colnames(axisData) <- c("axis.no","x","y")
    rownames(axisData) <- seq(1:nrow(axisData))
    #Return calculated axis paths
    as.data.frame(axisData)
  }
  funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
    #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  #
  #
  ### Convert supplied data into plottable format
  #
  plot.data.offset <- plot.data
  col.idx <- 1:ncol(plot.data)
  col.idx <- col.idx[-c(group_col, weight_col)]
  plot.data.offset[,col.idx]<- plot.data[,col.idx] + abs(centre.y)
  #
  #print(plot.data.offset)
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset, group_col = group_col, weight_col = weight_col)
  group$path$size <- sapply(as.numeric(as.character(as.vector(group$path$group))), (function(grp){
     if (!is.null(weight_col)) { 
       as.numeric(plot.data[plot.data[,group_col] == grp, "size"]) 
     } else { 
       0 
     }
  }))
  #
  #print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  #print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  #Labels
  axis.labels=colnames(plot.data)[-c(group_col, weight_col)]
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  #print(axis$label)
  #axis label coordinates
  #
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  #print(axis$label)
  # (e) Create Circular grid-lines + labels
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
  #print(head(gridline$max$path))
  #gridline labels
  gridline.label.offset <- -0.1*(grid.max-centre.y)
  gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                   text=as.character(grid.min))
  gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                   text=as.character(grid.max))
  gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                   text=as.character(grid.mid))
  #
  #
  #
  #print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  #print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  #Labels
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  #print(axis$label)
  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  #print(axis$label)
  # (e) Create Circular grid-lines + labels
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
  #print(head(gridline$max$path))
  #gridline labels
  gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                   text=as.character(grid.min))
  gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                   text=as.character(grid.max))
  gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                   text=as.character(grid.mid))
  #print(gridline$min$label)
  #print(gridline$max$label)
  #print(gridline$mid$label)
  ### Start building up the radar plot
  theme_clear <- theme_bw(base_size=20) +
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_blank())
  #
  theme_clear <- theme_clear + theme(legend.position=legend.pos)
  #
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
              aes(x=x,y=y,label=text),size=axis.label.size,hjust=1, family=font.radar) +
    scale_x_continuous(limits=c(-1.5*plot.extent.x,1.5*plot.extent.x)) +
    scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
  #
  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5, family=font.radar)
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0, family=font.radar)
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
                              fill=background.circle.colour,
                              alpha=background.circle.transparency)
  # + radial axes
  base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                           colour=axis.line.colour)
  #
  #... + amend Legend title
  #
  base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
                            lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
                            lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
                            lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)
  #
  base <- base + geom_text(aes(x=x,y=y,label=values.radar[1]),data=gridline$min$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
  base <- base + geom_text(aes(x=x,y=y,label=values.radar[2]),data=gridline$mid$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
  base <- base + geom_text(aes(x=x,y=y,label=values.radar[3]),data=gridline$max$label,size=grid.label.size*0.8, hjust=1, family=font.radar)
  #
  #
  # ... + group (cluster) 'paths'
  #
  if(!is.null(weight_col)){
    base_f <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=size),
                               alpha = linepoint.alpha,
                               size=group.line.width)
  } else {
    base_f <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                               alpha = linepoint.alpha,
                               size=group.line.width)
  }
  # ... + group points (cluster data)
  if (group.point.size > 0) {
    if(!is.null(weight_col)){
      base_f <- base_f + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=size),
                                    alpha = linepoint.alpha,
                                    size=group.point.size)
    } else {
        base_f <- base_f + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),
                                  alpha = linepoint.alpha,
                                  size=group.point.size)
    }
  }
  #
  base_f  <- base_f + theme(legend.text=element_text(size=10), legend.title = element_text(size = 12))
  base_f  <- base_f + theme(plot.title=element_text(size=18, face = "bold"))
  #
  return(base_f)
}
