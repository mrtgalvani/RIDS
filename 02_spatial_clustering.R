setwd('C:/Users/m.galvani/Desktop/RIDS/Terrorism/script')
rm(list=ls())
library(lubridate)
library(plyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

load('./Rome/dati/data.RData')

#### focus on Europe

#data <- data[data$continent=='Europe',]

#### CLUSTERING WITH Density Based Spatial Clustering ####
library(dbscan)

dataset <- as.matrix(data[c('longitude','latitude')])


kNNdistplot(dataset,k=10) # for europe 10
abline(h=0.5, col = "red", lty=2) # for europe 1

res <- dbscan(dataset, eps = 0.5, minPts = 10)

dataset <- as.data.frame(dataset)
dataset$nkill <- as.numeric(data$nkill)
dataset$cluster <- as.factor(res$cluster)

data$cluster <- as.factor(res$cluster)
summary(factor(data$cluster)) # for europe 50 cluster


library(rworldmap)
mapBubbles(dF=dataset, nameX ='longitude', nameY = 'latitude'
           ,nameZSize='nkill'
           ,nameZColour="cluster"
           ,colourPalette="rainbow"
           ,oceanCol="lightblue"
           ,landCol="wheat", legendPos = NULL)

p <- plot_geo(dataset) %>%
  add_markers(
    x = ~longitude, y = ~latitude, size = ~nkill, color = ~cluster)

ggplotly(p=p)

#################
library(ggmap)
Asia <- data[data$continent=='Asia',]
map <- get_map(location = 'Asia', zoom = 3)
mapPoints <- ggmap(map) +geom_point(aes(x = longitude, y = latitude, size = nkill,color=cluster), data = Asia, alpha = .5)
mapPoints 

Europe <- data[data$continent=='Europe',]
map <- get_map(location = 'Europe', zoom = 3)
mapPoints <- ggmap(map) +geom_point(aes(x = longitude, y = latitude, size = nkill,color=cluster), data = Europe, alpha = .5)
mapPoints 

Africa <- data[data$continent=='Africa',]
map <- get_map(location = 'Africa', zoom = 3)
mapPoints <- ggmap(map) +geom_point(aes(x = longitude, y = latitude, size = nkill,color=cluster), data = Africa, alpha = .5)
mapPoints 

America <- data[data$continent=='America',]
map <- get_map(location = 'North America', zoom = 3)
mapPoints <- ggmap(map) +geom_point(aes(x = longitude, y = latitude, size = nkill,color=cluster), data = America, alpha = .5)
mapPoints 

#### CLUSTERING WITH Density Based Spatial Clustering using time ####
# 
# dataset <- data[c('longitude','latitude','timestamp')]
# 
# time <- min(dataset$timestamp)
# dataset$timestamp_diff <- as.numeric(difftime(dataset$timestamp,time))
# 
# dataset <- as.matrix(dataset[-3])
# 
# kNNdistplot(dataset,k=4)
# abline(h=10000, col = "red", lty=2)
# 
# res <- dbscan(dataset, eps = 10000, minPts = 4)
# dataset <- as.data.frame(dataset)
# dataset$nkill <- as.numeric(data$nkill)
# dataset$cluster <- as.factor(res$cluster)
# 
# mapBubbles(dF=dataset, nameX ='longitude', nameY = 'latitude'
#            ,nameZSize='nkill'
#            ,nameZColour="cluster"
#            ,colourPalette="rainbow"
#            ,oceanCol="lightblue"
#            ,landCol="wheat", legendPos = NULL)
# 
# 
# p <- plot_geo(dataset) %>%
#   add_markers(
#     x = ~longitude, y = ~latitude, size = ~nkill, color = ~cluster)
# 
# ggplotly(p=p)

#### Biggest cluster ####

count<- data.frame(table(dataset$cluster))

data$cluster <- as.factor(res$cluster)

cl_12 <- data[data$cluster=='46',]

##oSS: i cluster prendono outlier strani in altre regioni
summary(factor(cl_12$country_txt))
summary(factor(cl_12$region_txt))

save(data, file='./Rome/dati/data_cluster.RData')

save(data, file='./Rome/dati/data_cluster_europe.RData')

count_event <- data.frame(table(data$timestamp,data$cluster))

ggplot(data=count_event,aes(x=strptime(Var1, format="%Y-%m-%d"),y=Freq,group=Var2,colour=Var2)) + 
  geom_line(size = 1) +
  #geom_point(size=2) + 
  xlab('') + ylab('Number of accidents') + scale_y_continuous() + ggtitle('Number of terrorist attacks per day per cluster') +
  theme_bw() + 
  theme(axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))

