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

p <- read.csv("Brahmputra_basin_data.csv")
t= dim(p)
mat<- matrix(0, nrow = t[2], ncol = 2, byrow = FALSE)
for(i in 1:(t[2]-1))
{
  m = mean(p[,i+1])
  mat[i,1]<-m
  s <- sd(p[,i+1])
  mat[i,2]<-s
}
write.csv(mat,'mean_sd.csv')

q <- read.csv("Bramhaputra_grid.csv")

chart<-list()
w<- c(62,90,180,220,300)

cols <- c("[62,90)"= "red", "[90,180)"= "blue", "[180,220)"="darkgreen","[220,300)"= "darkorange")
q$A1 <- cut(q$Mean,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y)) + geom_tile(aes(fill = A1)) +
  scale_fill_manual( values = cols, aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[1]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Mean of River Basin")


cols <- c("[62,90)"= "red", "[90,180)"= "blue", "[180,220)"="darkgreen","[220,300)"= "darkorange")
q$A2 <- cut(q$Std,breaks = w,right = FALSE)
h <- ggplot(data = q, aes(x = x, y = y)) +
  geom_tile(aes(fill = A2)) +
  scale_fill_manual(values = cols, aesthetics = c("colour", "fill"))+theme_classic()


River_B <- readShapeSpatial("11-9-2019-3772456")
ch <- geom_polygon(data=River_B, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="Std of River Basin")



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
tiff(filename = "Brahmaputra.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[1]], chart[[2]], cols = 2)
dev.off()
jpeg(filename = "Brahmaputra.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[1]], chart[[2]], cols = 2)
dev.off()