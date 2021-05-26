setwd("C:/Users/anush/Downloads/Data vis _lab/Data vis _lab")
par(mar = c(10,10,10,10))
#par(mfrow=c(1,4))
library(ggplot2)
library(grid)
library(gridExtra)
library(extrafont)
library(ggmap)
library(maptools)
library(plyr)
library(maps)


q <- read.csv("India_grids.csv")

chart<-list()
w<- c(-49,0,50,68)

cols <- c("[-49,0)"= "red", "[0,50)"= "blue", "[50,68)"="darkgreen")
q$A1 <- cut(q$q3,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y)) + geom_tile(aes(fill = A1)) +
  scale_fill_manual( values = cols, aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("Ind")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Average Seasonal Rainfall")



##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols=2) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "India2.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[1]], cols = 1)
dev.off()
jpeg(filename = "India2.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[1]], cols = 1)
dev.off()