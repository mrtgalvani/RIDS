setwd('C:/Users/m.galvani/Desktop/RIDS/Terrorism/script')
rm(list=ls())
library(lubridate)
library(plyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

load('./Rome/dati/data.RData')

data <- data[data$continent=='Europe',]
data <- data[data$country_txt=='Italy',]

dataset <- data[c('eventid','region_txt','provstate','latitude','longitude','nkill','nwound',
                  'attacktype1_txt','targtype1_txt','suicide')]

dataset$nwound <- as.numeric(dataset$nwound)
cols <- c('eventid','region_txt','provstate',
          'attacktype1_txt','targtype1_txt','suicide')
dataset[cols] <- lapply(dataset[cols], factor)

#### distances matrix ####

library(cluster)

gower_dist <- daisy(dataset[-1],
                    metric = "gower")
#### Silhouette ####

sil_width_pam <- c(NA)

for(i in 2:20){
  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)
  sil_width_pam[i] <- pam_fit$silinfo$avg.width
}

sil_width_pam <- data.frame(sil_width_pam)
sil_width_pam$k <- row.names(sil_width_pam)
sil_width_pam$type <- 'K_medoids'
names(sil_width_pam)[1] <- 'Sil'

sil_width_hclust <- c(NA)
res.hc <- hclust(gower_dist, method = "ward.D" )

for(i in 2:20){
  
  grp <- cutree(res.hc, k = i)
  sil_width_hclust[i] <-  summary(silhouette(grp, gower_dist))$si.summary[3]
}

sil_width_hclust <- data.frame(sil_width_hclust)
sil_width_hclust$k <- row.names(sil_width_hclust)
sil_width_hclust$type <- 'h_clust'
names(sil_width_hclust)[1] <- 'Sil'

# Plot sihouette width (higher is better)
sill <- rbind(sil_width_hclust,sil_width_pam)

library(ggplot2)
ggplot(sill, aes(x=as.integer(k),y=Sil,group=type,color=type)) + 
  geom_path() + geom_point() 

#### clustering ####
res.hc <- hclust(gower_dist, method = "ward.D" )

# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = 7, border = 2:5) # add rectangle

source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

### colored dendrogram
op = par(bg = "#EFEFEF")
windows()
A2Rplot(res.hc, k = 7, boxes = FALSE, col.up = "gray50", col.down = c('deeppink','turquoise2','darkorange','lawngreen','lightskyblue1','dodgerblue2','yellow','orange4','tomato2','pink','purple','darkgreen'))


grp <- cutree(res.hc, k = 7)

summary(factor(grp))

dataset$CLUSTER <- as.factor(grp)

save(dataset, file='./Rome/dati/dataset_gower_cluster.RData')

#### GRAFICI ####

library(psych)
library(gmodels)

for(i in 2:ncol(dataset)){
  if(class(dataset[1,i])=='factor' | class(dataset[1,i])=='character'){
    name <- names(dataset)[i]
    jpeg(file = paste0('./../img/cluster/',name,'.jpeg'))
    crosstable <- CrossTable(dataset$CLUSTER,as.matrix(dataset[i]))
    crosstable_row <- data.frame(as.matrix(crosstable$prop.row))
    crosstable_col <- data.frame(as.matrix(crosstable$prop.col))
    
    g <- ggplot(crosstable_row, aes(x=x,y=Freq, fill=y)) + geom_bar(stat='identity') + ggtitle(name) +
      theme_bw()
    print(g)
    dev.off()
    
    
  }
  else {
    name <- names(dataset)[i]
    jpeg(file = paste0('./../img/cluster/',name,'.jpeg'))
    data <- dataset[c(name,'CLUSTER')]
    names(data) <- c('VAR','CLUSTER')
    g <- ggplot(data, aes(x=VAR, fill=CLUSTER)) + geom_histogram(aes(y = ..density..)) + theme_bw() + ggtitle(name)
    g <- ggplot(data, aes(x=VAR, fill=CLUSTER))  + geom_density(alpha = 0.5) + theme_bw() + ggtitle(name)
    g <- ggplot(data, aes(x=CLUSTER,y=VAR, fill=CLUSTER))  + geom_boxplot() + theme_bw() + ggtitle(name)
    print(g)
    dev.off()
  }
}

library(ggmap)
Europe <- dataset
map <- get_map(location = 'Europe', zoom = 3)
mapPoints <- ggmap(map) +geom_point(aes(x = longitude, y = latitude, size = nkill,color=CLUSTER), data = Europe, alpha = .5)
mapPoints 

crosstable <- CrossTable(dataset$CLUSTER,dataset$targtype1_txt)
crosstable_row <- data.frame(as.matrix(crosstable$prop.row))
crosstable_col <- data.frame(as.matrix(crosstable$prop.col))

g <- ggplot(crosstable_col, aes(x=y,y=Freq, fill=x)) + geom_bar(stat='identity') + ggtitle(name) +
  theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
g



