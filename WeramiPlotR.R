#
#
#  WeramiPlotR for plotting 2D werami results
#  by R. Fukushima (v0.1; Jan 14, 2026)
#
#  This script requires 'fields' and 'matlab' packages 
#
#
library(fields)
library(matlab)
################################################################################
# Set a path to the .tab file generated with werami

data <- read.table("tl04_fea_v5_2.tab", sep = "\t")

####################
# Do not edit this section

plotteddata <- data.frame()
splitted <- list()


for (i in 14:nrow(data)){
  
  splitted <- unlist(strsplit(data[i,]," +"))
  plotteddata <- rbind(plotteddata, splitted[2:length(splitted)])
  
}


resolution <- as.numeric(unlist(strsplit(data[7,]," +"))[2])

Xmin <- as.numeric(unlist(strsplit(data[5,]," +"))[2])
Xint <- as.numeric(unlist(strsplit(data[6,]," +"))[2])
Xmax <- Xmin + (Xint * (resolution - 1))

Ymin <- as.numeric(unlist(strsplit(data[9,]," +"))[2])
Yint <- as.numeric(unlist(strsplit(data[10,]," +"))[2])
Ymax <- Ymin + (Yint * (resolution - 1))

colnames(plotteddata) <- unlist(strsplit(data[13,]," +"))

################################################################################
# See 'plotteddata' and choose the parameter column for plotting

PlotColumnNo <- 4

####################

mat <- matrix(plotteddata[,PlotColumnNo], nrow=resolution, ncol=resolution)
mat <- apply(mat, c(1,2), as.numeric)

x <- seq(Xmin, Xmax ,by=Xint)
y <- seq(Ymin, Ymax ,by=Yint)

par(pin = c(4,4))
image.plot(x,y,mat,col=jet.colors(32), xlab = "", ylab = "", legend.args = list(text = colnames(plotteddata)[PlotColumnNo], side = 3, line = 0.5, cex = 0.8))

par(new = T)
contour(mat, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")


