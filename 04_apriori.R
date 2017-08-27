setwd('C:/Users/m.galvani/Desktop/RIDS/Terrorism/Script')
rm(list=ls())

source("./function.R")


load('./Rome/dati/data_cluster_europe.RData')
#load('./Rome/dati/data_gower_cluster.RData')

dataset <- data
dataset$event_det <- paste0(dataset$targtype1_txt,'-',dataset$attacktype1_txt)

#################
#### apriori ####
#################
library(arules)

dati <- as.data.frame(paste0(dataset$provstate,',',dataset$targtype1_txt,',',dataset$attacktype1_txt))
names(dati) <- 'attack'
dati$attack <- as.character(dati$attack)
dati$attack <- gsub(',Unknown','',dati$attack)
names(dati) <- NA
write.csv(dati,"ItemList.csv",quote=F,row.names=F)

trans <-  read.transactions(file="ItemList.csv", rm.duplicates= TRUE, format="basket",sep=',')

rules <- apriori(trans,parameter = list(sup = 0.05, conf = 0.5,target="rules"))
inspect(rules)
save_rules <- as(rules, "data.frame")

library(arulesViz)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules,measure=c("support","lift"),shading="confidence",interactive=T)


rules <- apriori(trans,parameter = list(sup = 0.05, conf = 0.5, target="frequent itemsets", minlen=2))
inspect(rules)
save <- as(rules, "data.frame")

#### CREATE PLOT AS OPTs

save$group <- row.names(save)
save$items <- gsub('\\{','',save$items)
save$items <- gsub('\\}','',save$items)

library(splitstackshape)

save <- cSplit(save,1,sep=',',direction='long')
save <- save[,c(3,1)]
names(save) <- c('cluster','node')

## remove subset of other set and plot as optional

save <- calcola_num(save)
save <- save[order(save$num,decreasing=T),]
save <- relabel_clusters(save)
save <- remove_subsets_2(save)
save <- relabel_clusters(save)

plotCommunities(save)

##############################
#### apriori per clusters ####
#############################

library(arules)
count <- data.frame(table(dataset$cluster)) 
# 21 Ukrain 2004-08-24 to 2015-12-26
# 2 Russia 2001-09-13 to 2015-12-29
# 4 Ireland 2001-09-23 to 2015-12-31
# 9 Greece 2001-11-15 to 2015-12-16
# 7 France 2001-10-22 to 2015-12-24

data <- dataset[dataset$cluster==4,]
summary(factor(data$country_txt))
min(data$timestamp)
max(data$timestamp)

dati <- as.data.frame(paste0('location - ',data$provstate,',',
                             'target - ',data$targtype1_txt,',',
                             'type - ',data$attacktype1_txt))
names(dati) <- 'attack'
dati$attack <- as.character(dati$attack)
dati$attack <- gsub(',Unknown','',dati$attack)
names(dati) <- NA
write.csv(dati,"ItemList.csv",quote=F,row.names=F)

trans <-  read.transactions(file="ItemList.csv", rm.duplicates= TRUE, format="basket",sep=',')
rules <- apriori(trans,parameter = list(sup = 0.1, conf = 0.5, target="frequent itemsets", minlen=2))
inspect(rules)

save <- as(rules, "data.frame")
save <- save[order(save$support),]

#### CREATE PLOT AS OPTs

save$group <- row.names(save)
save$items <- gsub('\\{','',save$items)
save$items <- gsub('\\}','',save$items)

library(splitstackshape)

save <- cSplit(save,1,sep=',',direction='long')
save <- save[,c(3,1)]
names(save) <- c('cluster','node')

## remove subset of other set and plot as optional

save <- calcola_num(save)
save <- save[order(save$num,decreasing=T),]
save <- relabel_clusters(save)
save <- remove_subsets_2(save)
save <- relabel_clusters(save)

plotCommunities(save)

############################
#### sequential pattern ####
############################

library(arulesSequences)
library(TraMineR)

## sequenze per mese

# dati <- dataset[c('timestamp','event_det')]
# dati$MY <- format(as.Date(dataset$timestamp), "%Y-%m")
# 
# dati <- dati[order(dati$timestamp),]
# 
# dati$eventID <- ave(dati$MY, dati$MY, FUN=seq_along)
# dati$sequenceID <- as.character(dati$MY)

dati <- dataset[c('timestamp','event_det','cluster')]

dati <- dati[order(dati$timestamp),]

dati$cluster <- as.numeric(as.character(dati$cluster))

dati$eventID <- ave(dati$cluster, dati$cluster, FUN=seq_along)
dati$sequenceID <- dati$cluster +1 #as.character(dati$cluster)

seq <- dati[c('event_det','sequenceID','eventID')]
seq$SIZE <- 1
names(seq)[1] <- 'items'

seq$items <- gsub(' ','_',seq$items)
#seq$items <- paste0('{',seq$items,'}')

seq$eventID <- as.numeric(as.character(seq$eventID))
seq <- seq[order(seq$sequenceID,seq$eventID),]

seq <- seq[c(2,3,4,1)]

#write.csv(seq,"ItemList.csv",quote=F,row.names=F)
write.table(seq, "input_file.txt", row.names = F, col.names = F)

x <- read_baskets(con = "input_file.txt",  info = c("sequenceID","eventID","SIZE"))
as(x, "data.frame")

s1 <- cspade(x, parameter = list(support = 0.3), control = list(verbose = TRUE))
summary(s1)

as(s1, "data.frame")

save <- as(s1, "data.frame")

### plot most frequent sequences

save$sequence <- as.character(save$sequence)
save$sequence <- gsub('\\{','',save$sequence)
save$sequence <- gsub('\\}','',save$sequence)
save$sequence <- gsub('<','',save$sequence)
save$sequence <- gsub('>','',save$sequence)
save$sequence <- gsub('\\"','',save$sequence)



#save$freq <- round(save$support*100)

seq <- seqdef(save, var=1, stsep=',',weights=save$freq)

attack <- data.frame(alphabet(seq))
attack$colors <-   c('dodgerblue','chartreuse','darkgreen','gray','red','pink','darkmagenta',
                     'yellow','lightsalmon2','blue','olivedrab3','darkorange','royalblue3','deeppink',
                     'orange','azure','yellow2','darkorchid1','greenyellow','lightgoldenrod3' )

cpal(seq) <- attack$colors



seqfplot(seq,withlegend=FALSE,tlim=1:50)

seqIplot(seq, withlegend=FALSE,sortv = "from.start")

seq <- seq[seq$`[2]`!='%',]
seqiplot(seq, withlegend=FALSE)

seqlegend(seq,fontsize=0.5)

### Sankey
library(googleVis)
library(dplyr)
library(reshape2)

#orders <- dcast(orders, id_party ~ n.ord, value.var='type', fun.aggregate = NULL)

attacks <- as.character(save$sequence)
attacks <- strsplit(attacks, ",")

# find the largest element
maxLen <- max(sapply(attacks, length))

# fill in any blanks. The t() is to transpose the return from sapply
attacks <-  t(sapply(attacks, function(x) c(x, rep(NA, maxLen - length(x)))))

# add column names as necessary
colnames(attacks) <- paste(c("1st", "2nd", "3rd","4th",'5th','6th','7th','8th','9th','10th'), "Attack")
colnames(attacks) <- gsub('X','',colnames(attacks))

# Put it all back together
save1 <- data.frame(attacks, freq=save$support)
save1 <- save1[order(save1$freq, decreasing = T),]

write.csv(save1, file='./Rome/dati/sequential_arules.csv')

save1$freq <- round(save1$freq*100)
orders <- save1[rep(row.names(save1), save1$freq), 1:10]

orders.plot <- data.frame()
#i <- 2
for (i in 2:ncol(orders)) {
  
  ord.cache <- orders %>%
    group_by(as.factor(orders[,i-1]), as.factor(orders[,i])) %>%
    summarise(n=n())
  
  colnames(ord.cache)[1:2] <- c('from', 'to')
  
  # adding tags to carts
  ord.cache$from <- paste(ord.cache$from, '(',i-2, ')', sep='')
  ord.cache$to <- paste(ord.cache$to, '(',i-1, ')', sep='')
  
  orders.plot <- rbind(as.data.frame(orders.plot), as.data.frame(ord.cache))
  
}

plot(gvisSankey(orders.plot, from='from', to='to', weight='n',
                options=list(
                  sankey="{link: {color: { fill: '#d799ae' } },
                  node: { color: { fill: '#a61d4c' },
                  label: { color: '#871b47' } }}")))
##################
#### traminer ####
##################

library(TraMineR)
library(plyr)
seq <- ddply(dataset,.(cluster), summarize,
             event = paste(event_det,collapse='>'))

df.seq <- seqdef(seq, var=2, stsep='>')

seqdplot(df.seq, group=df.feat$sex)

######################
#### Markov Chain ####
######################

library(markovchain)






