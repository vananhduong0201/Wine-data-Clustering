rm(list=ls())

library(dplyr)
library(ggplot2)
library(NbClust)
library(purrr)
library(factoextra)
# Load the dataset
Wine<-read.csv('Wine_data.csv', header=TRUE, sep=',')
str(Wine)
summary(Wine)

# check if there is any missing/null/NA/duplicated value in the data
any(is.na(Wine)) # There are NA values in the dataset
any(is.null(Wine))
duplicated(Wine)

# Replace NA value to mean value
Wine=Wine[complete.cases(Wine),]
summary(Wine)

# Basic Plotting
par(mfrow=c(2,4))
barplot(table(Wine$TYPE), main="Type")
hist(Wine$ALCOHOL, main='ALCOHOL')
hist(Wine$ALCALINITY, main='ALCALINITY')
hist(Wine$MAGNESIUM, main='MAGNESIUM')
hist(Wine$MALIC, main='MALIC')
hist(Wine$ASH, main='ASH')
hist(Wine$COLOR, main='COLOR')
hist(Wine$PROLINE, main='PROLINE')

par(mfrow=c(2,3))
hist(Wine$PHENOLS, main='PHENOLS')
hist(Wine$FLAVANOIDS, main='FLAVANOIDS')
hist(Wine$NONFLAVANOIDS, main='NONFLAVANOIDS')
hist(Wine$PROANTHOCYANINS, main='PROANTHOCYANINS')
hist(Wine$HUE, main='HUE')
hist(Wine$ASH, main='ASH')


# Checking for outliers
par(mfrow=c(1,1))
boxplot(Wine[,c(1:7)],main="Box Plot Analysis",xlab="", ylab="value")
boxplot(Wine[,8:14],main="Box Plot Analysis",xlab="", ylab="value")
boxplot(Wine[,c(6,14)],main="Box Plot Analysis",xlab="", ylab="value")

# Checking correlation
corrplot(cor(Wine), "ellipse")
plot(Wine, col='darkred')

# Hierachecal clustering

dismat<-dist(Wine,method='euclidean')
hmdl<-hclust(dismat,method='complete')

Dmodel<-as.dendrogram(hmdl)
plot(Dmodel, main="Dendrogram Model")

clustermember=cutree(hmdl, k=3)
Wine_2<-Wine %>% mutate(member=clustermember)

# Visualize clustering 
ggplot(Wine_2, aes(x=PROLINE, y =ALCOHOL, col=as.factor(member)))+
  geom_point()+
  ggtitle('Proline vs Alcohol')

fviz_dend(Dmodel, k = 3, # Cut in three groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

# Kmeans
kmeansmdl=kmeans(Wine, centers=3, nstart=25)

# Plot all variables to visuallized the clusters
plot(Wine, col=kmeansmdl$cluster,pch=16)

#Clusters are visible between PROLINE and other varibles, we can check one example
ggplot(Wine, aes(x=PROLINE, y=COLOR))+
  geom_point(col=kmeansmdl$cluster)+
  ggtitle('Cluster plot: Proline vs Color')

kmeansmdl$tot.withinss
kmeansmdl$withinss
kmeansmdl$betweenss/kmeansmdl$totss

# Elbow
tot_withinss=map_dbl(1:10,function(k){
  model=kmeans(Wine,centers=k, nstart=25)
  model$tot.withinss
})

plot(1:10, tot_withinss,type='o', main ='Elbow method')

# Other methods
SilClust<- NbClust(Wine, distance='euclidean', min.nc=2,max.nc=10,method='kmeans', index='silhouette')
GapClust<- NbClust(Wine, distance='euclidean', min.nc=2,max.nc=10,method='kmeans', index='gap')

par(mfrow=c(1,3))
plot(1:10, tot_withinss,type='o', main ='Elbow method')
plot(2:10,SilClust$All.index, type='o', main = 'Silhouette method')
plot(2:10,GapClust$All.index, type='o', main = 'Gap method')

# Summary all data
Wine_2<-Wine %>% mutate(member=kmeansmdl$cluster)

Wine_2 %>% 
  group_by(member) %>%
  summarise_all(list(avg=mean))

Wine_2 %>% 
  group_by(member) %>%
  summarise_all(list(sd = sd))

#################********
### Checking outliers
outliers <- boxplot(Wine, plot=FALSE)$out

# Remove extreme outliers in MAGNESIUM, COLOR
Wine_new <- Wine[-which(Wine$MAGNESIUM %in% outliers),]
Wine_new <- Wine_new[-which(Wine_new$COLOR %in% outliers),]
summary(Wine_new)

par(mfrow=c(1,1))
boxplot(Wine_new[,1:13],main="Wine_new",xlab="", ylab="value")

#Kmeans
kmeansmdl_3=kmeans(Wine_new, centers=3, nstart=25)

plot(Wine_new, col=kmeansmdl_3$cluster,pch=16)

ggplot(Wine_new, aes(x=PROLINE, y=MAGNESIUM))+
  geom_point(col=kmeansmdl_3$cluster)+
  ggtitle('Cluster plot: Proline vs MAGNESIUM')

kmeansmdl_3$tot.withinss

tot_withinss_3=map_dbl(1:10,function(k){
  model=kmeans(Wine_new,centers=k, nstart=25)
  model$tot.withinss
})

# Compare the different 

Tot_withinss_dif = tot_withinss-tot_withinss_3
plot(1:10,Tot_withinss_dif/1000,type='o', col='red', main='', xlab='clusters', ylab= 'distance in 1000s')





