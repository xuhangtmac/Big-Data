happiness<-read.csv("~/Desktop/whr_2017.csv")


library(tidyverse)   
library(ggplot2)     
library(readr)      
library(tidyr) 
library(plyr)
library(factoextra)
library(car)
library(GGally)


# Input some countries' missing GDP values
happiness[139,9]<-2325.07
happiness[23,9]<-647.8804
happiness[88,9]<-5305.047
happiness[4,9]<-18489.43
happiness[80,9]<-35705.1
happiness$LnGDPpc<-log(happiness$GDPpc,exp(1))

#Fill out NA with Average values
for(i in c(2,4,5,6,7,8)){
  happiness[,i][is.na(happiness[,i])] <- mean(happiness[,i], na.rm = TRUE)
}

#Take out the rows with missing values
happiness<-na.omit(happiness)

#Draw a correlation matrix
ggcorr(happiness[, -c(1,9)], label=TRUE, cex=2)

#Draw density graphs to see the distrbution of all the variables
opar<-par(no.readonly = TRUE)
par(mfrow=c(3, 4))
par(mar = rep(2, 4))

for (i in 2:9) {
  d <- density(happiness[,i])
  plot(d, type="n", main=colnames(happiness)[i])
  polygon(d, border="gray")}

par(opar)

powerTransform(happiness[,c(4,6,8)])
happiness$PwrSocSupp<-(happiness$SocSupp)^4.06
happiness$PwrLifeChoice<-(happiness$LifeChoice)^2.93
happiness$PwrCorruption<-(happiness$Corruption)^2.84

opar<-par(no.readonly = TRUE)
par(mfrow=c(3, 4))
par(mar = rep(2, 4))

for (i in 2:12) {
  d <- density(happiness[,i])
  plot(d, type="n", main=colnames(happiness)[i])
  polygon(d, border="gray")}

par(opar)

# select variables after analysis
happiness1<-happiness[,c(1,2,3,5,7,10,11,12)]

# normalize input variables
happiness.norm <- data.frame(sapply(happiness1[,-1], scale))

# add row names
row.names(happiness.norm) <- happiness1[,1]

opar<-par(no.readonly = TRUE)
par(mfrow=c(3,4))
par(mar = rep(2, 4))

for (i in 1:7) {
  d <- density(na.omit(happiness.norm[,i]))
  plot(d, type="n", main=colnames(happiness.norm)[i])
  polygon(d, border="gray")}

par(opar)

### exclude lifeLadder
happiness.norm2<-happiness.norm[,-1]

## hierarchical cluster method  
# compute Euclidean distance
d.norm <- dist(happiness.norm2, method = "euclidean")

# choose the optimal k based on WSS
fviz_nbclust(happiness.norm2, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

#Compare average and complete method
hc1 <- hclust(d.norm, method = "average")
plot(hc1, hang = -1, ann = FALSE,cex=0.45)
memb1 <- cutree(hc1, k = 3)
memb1
fviz_cluster(list(data = happiness.norm2, cluster = memb1))
table(memb1)

hc2 <- hclust(d.norm, method = "complete")
memb2 <- cutree(hc2, k = 3)
fviz_cluster(list(data = happiness.norm2, cluster = memb2))
table(memb2)
# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(happiness.norm2), dendrogram="row",Colv = NA, hclustfun = hclust,key.xlab = "Cm", 
        col=rev(paste("gray",1:99,sep="")), cexRow = 0.3,cexCol = 0.8)


## kmeans method
# choose the optimal k based on WSS
set.seed(123)
fviz_nbclust(happiness.norm2, kmeans, method = "wss")
km <- kmeans(happiness.norm2, 3)
fviz_cluster(km, happiness.norm2)

km
x<-km$cluster
write.csv(x, file="~/Desktop/2017.csv")



# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8),cex.lab=0.4)

# label x-axes
axis(1, at = c(1:6), labels = names(happiness.norm2))

# plot centroids
for (i in c(1:3))
  lines(km$centers[i,], lty = i, lwd = 2, col = i)
# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:3)))



### include lifeLadder
d.norm1 <- dist(happiness.norm, method = "euclidean")

# choose the optimal k based on WSS
fviz_nbclust(happiness.norm, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

#Compare average and complete method
hc3 <- hclust(d.norm1, method = "average")
plot(hc3, hang = -1, ann = FALSE,cex=0.45)
memb3 <- cutree(hc3, k = 3)
table(memb3)
fviz_cluster(list(data = happiness.norm, cluster = memb3))

hc4 <- hclust(d.norm1, method = "complete")
memb4 <- cutree(hc4, k = 3)
table(memb4)
memb4
fviz_cluster(list(data = happiness.norm, cluster = memb4))

# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(happiness.norm), dendrogram="row",Colv = NA, hclustfun = hclust,key.xlab = "Cm", 
        col=rev(paste("gray",1:99,sep="")), cexRow = 0.3,cexCol = 0.8)

## kmeans method
# choose the optimal k based on WSS
set.seed(123)
fviz_nbclust(happiness.norm, kmeans, method = "wss")
km1 <- kmeans(happiness.norm, 3)
fviz_cluster(km1, happiness.norm)
km1
x1<-km1$cluster
write.csv(x1, file="~/Desktop/2018.csv")

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km1$centers), max(km1$centers)), xlim = c(0, 8),cex.lab=0.4)

# label x-axes
axis(1, at = c(1:7), labels = names(happiness.norm))

# plot centroids
for (i in c(1:3))
  lines(km1$centers[i,], lty = i, lwd = 2, col = i)
# name clusters
text(x = 0.5, y = km1$centers[, 1], labels = paste("Cluster", c(1:3)))

