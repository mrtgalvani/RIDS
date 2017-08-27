setwd('C:/Users/m.galvani/Desktop/RIDS/Terrorism/Script')
rm(list=ls())
library(lubridate)
library(plyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

data <- read.csv("./Rome/dati/data.csv",
                 header=T, sep = ";", na.strings = "NULL",stringsAsFactors = F)

data[data==''] <- NA

data$timestamp <- paste0(data$iyear,'-',data$imonth,'-',data$iday)
data$timestamp <- as.POSIXct(data$timestamp, format="%Y-%m-%d",tz="GMT")

data <- data[which(data$timestamp >= as.POSIXct('2001-09-11', format="%Y-%m-%d",tz="GMT")),]

data <- subset(data, select =c('eventid','timestamp','country_txt','region_txt','provstate','city','latitude','longitude',
                               'vicinity','crit1','crit2','crit3','multiple','success','suicide',
                               'attacktype1_txt','attacktype2_txt','attacktype3_txt',
                               'targtype1_txt','targsubtype1_txt','target1','natlty1_txt',
                               'targtype2_txt','targsubtype2_txt','target2','natlty2_txt',
                               'targtype3_txt','targsubtype3_txt','target3','natlty3_txt',
                               'gname','gsubname','gname2','gsubname2','gname3','gsubname3',
                               'nperpcap','claimed','claimmode_txt','claim2','claimmode2_txt','claim3','claimmode3_txt',
                               'weaptype1_txt','weapsubtype1_txt',
                               'weaptype2_txt','weapsubtype2_txt',
                               'weaptype3_txt','weapsubtype3_txt',
                               'nkill','nwound','property','propextent_txt','propvalue',
                               'ishostkid','nhostkid','nhours','ndays','ransom','ransomamt','ransompaid', 'hostkidoutcome_txt',
                               'INT_LOG','INT_IDEO','INT_MISC','INT_ANY'))

data$continent <- unlist(sapply(data$region_txt,function(x){if(grepl('Europe',x)){'Europe'} else if(grepl('Asia',x)){'Asia'} else if(grepl('Africa',x)){'Africa'} else if(grepl('America',x)){'America'} else{'Oceania'}}))
data$region_txt <- paste0(data$continent,' - ',data$region_txt)

save(data, file='./Rome/dati/data.RData')
##########################################################################?
######### serie temporale per continente e numero di eventi ################
#########################################################################
#load('./Rome/dati/data_cluster_europe.RData')
#data <- data[data$country_txt=='Italy',]
count_event <- data.frame(table(data$timestamp,data$continent))

g <- ggplot(data=count_event,aes(x=strptime(Var1, format="%Y-%m-%d"),y=Freq,group=Var2)) + 
  geom_line(size = 1)  +
  #geom_point(size=2) + 
  xlab('') + ylab('Number of accidents') + scale_y_continuous() + ggtitle('Number of terrorist attacks per day per continent') +
  theme_bw() + 
  theme(axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
p <-ggplotly(p=g)

htmlwidgets::saveWidget(as.widget(p), file = "attacks_per_continent.html")

## il numero di attentati cresce notevolmente negli ultimi anni

### costruire le rette di approssimazione lineare per continente per vedere la pendenza della crescita

## Africa
#Africa <- lm(as.numeric(count_event$Freq[which(count_event$Var2=='Africa')]) ~ as.numeric(count_event$Var1[which(count_event$Var2=='Africa')]), data = count_event)

### per numero di morti
names(count_event) <- c('timestamp','continent','Freq')
count_event$continent <- as.character(count_event$continent)
count_event$timestamp <- as.character(count_event$timestamp)
data$nkill <- as.numeric(data$nkill)
sub <- data[c('timestamp','continent','nkill')]
sub$timestamp <- as.character(sub$timestamp)
count_death <- merge(count_event,sub,by=c('timestamp','continent'),all.x=T)
count_death$nkill[is.na(count_death$nkill)] <- 0

### grafico per severity nel tempo
g <- ggplot(data=count_death,aes(x=strptime(timestamp, format="%Y-%m-%d"),y=nkill,group=continent,colour=continent)) + 
  geom_line(size = 1) +
  #geom_point(size=2) + 
  xlab('') + ylab('Severity (number of deaths)') + scale_y_continuous() + ggtitle('Terrorist attacks severity per day per continent') +
  theme_bw() + 
  theme(axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))

p <- ggplotly(p=g)
htmlwidgets::saveWidget(as.widget(p), file = "severity_per_continent.html")

## il numero di morti resta pi? o meno costante tranne alcuni casi isolati e poco prima del 2015

### grafico per gravity (numero di morti fratto numero di attentati per giorno)
library(plyr)
count_death <- ddply(count_death, .(timestamp,continent,Freq),
                     summarize,
                     nkill = sum(nkill))

count_death$ser <- count_death$nkill/count_death$Freq

g <- ggplot(data=count_death,aes(x=strptime(timestamp, format="%Y-%m-%d"),y=ser,group=continent,colour=continent)) + 
  geom_line(size = 1) +
  #geom_point(size=2) + 
  xlab('') + ylab('Number of death \ Number of attacks') + scale_y_continuous() + ggtitle('Seriousness of terrorist attacks per day per continent') +
  theme_bw() + 
  theme(axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
p <- ggplotly(p=g)
htmlwidgets::saveWidget(as.widget(p), file = "seriuseness_per_continent.html")
## si nota come un volta fossero meno attentati ma pi? mirati e con pi? morti, mentre recentemente sono molti di pi? ma di piccola intensit?

##########################################################
############### serie temporale per regioni #################
#########################################################

count_event <- data.frame(table(data$timestamp,data$continent,data$region_txt))

g <- ggplot(data=count_event,aes(x=strptime(Var1, format="%Y-%m-%d"),y=Freq,group=Var3,colour=Var3)) + 
  geom_line(size = 1) + facet_wrap(~ Var2) + #scale_colour_manual(values=c('green','dodgerblue')) +
  xlab('') + ylab('Number of accidents') + scale_y_continuous() + ggtitle('Number of terrorist attacks per day per region') +
  theme_bw() + 
  theme(axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
p <- ggplotly(p=g)
htmlwidgets::saveWidget(as.widget(p), file = "attack_per_region.html")

### per numero di morti
names(count_event) <- c('timestamp','continent','region_txt','Freq')
count_event$continent <- as.character(count_event$continent)
count_event$timestamp <- as.character(count_event$timestamp)
data$nkill <- as.numeric(data$nkill)
sub <- data[c('timestamp','continent','region_txt','nkill')]
sub$timestamp <- as.character(sub$timestamp)
count_death <- merge(count_event,sub,by=c('timestamp','continent','region_txt'),all.x=T)
count_death$nkill[is.na(count_death$nkill)] <- 0

### grafico per severity nel tempo
g <- ggplot(data=count_death,aes(x=strptime(timestamp, format="%Y-%m-%d"),y=nkill,group=region_txt,colour=region_txt)) + 
  geom_line(size = 1) + facet_wrap(~ continent) + #scale_colour_manual(values=c('green','dodgerblue')) +
  #geom_point(size=2) + 
  xlab('') + ylab('Severity (number of deaths)') + scale_y_continuous() + ggtitle('Terrorist attacks severity per day per continent') +
  theme_bw() + 
  theme(axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
ggplotly(p=g)

### grafico per seriouseness (numero di morti fratto numero di attentati per giorno)
count_death <- ddply(count_death, .(timestamp,continent,region_txt,Freq),
                     summarize,
                     nkill = sum(nkill))

count_death$ser <- count_death$nkill/count_death$Freq
count_death$ser[is.na(count_death$ser)] <- 0

g <- ggplot(data=count_death,aes(x=strptime(timestamp, format="%Y-%m-%d"),y=ser,group=region_txt,colour=region_txt)) + 
  geom_line(size = 1) + facet_wrap(~ continent) +
  #geom_point(size=2) + 
  xlab('') + ylab('Number of death \ Number of attacks') + scale_y_continuous() + ggtitle('Seriousness of terrorist attacks per day per continent') +
  theme_bw() + 
  theme(axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
ggplotly(p=g)

#################################################
###### grafici su gruppi responsabili #############
#####################################################

gname_data <- data.frame(table(data$gname,data$continent))
gname_data <- gname_data[order(gname_data$Freq,decreasing = T),]
gname_data <- gname_data[1:15,]
ggplot(gname_data, aes(reorder(Var1,Freq),Freq, fill=Var2)) + geom_bar(stat="identity") +  ylab("Count") + xlab("Group name") +
  theme_bw() + ggtitle('Number of attacks per top 15 responsable groups') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
        axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))

### per ogni region plotto i primi 5 gruppi
gname <- data.frame(table(data$region_txt, data$gname))
gname <- gname %>%
  group_by(Var1) %>%
  arrange(desc(Freq)) %>%
  slice(1:6)
length(unique(gname$Var2))
g <- ggplot(gname, aes(Var1,Freq, fill=Var2)) + geom_bar(stat="identity") +  ylab("Count") + xlab("Group name") +
  theme_bw() + ggtitle('Number of attacks per region and responsable group') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
        axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
ggplotly(g)

p <- ggplotly(p=g)
htmlwidgets::saveWidget(as.widget(p), file = "group_per_region.html")

#######################################################
############## tipologia di attacco #################
######################################################

attack_type <- data.frame(table(data$attacktype1_txt,data$continent))
attack_type <- attack_type[order(attack_type$Freq,decreasing = T),]
ggplot(attack_type, aes(reorder(Var1,Freq),Freq,fill=Var1)) + geom_bar(stat="identity") +  ylab("Count") + xlab("Group name") +
  theme_bw() + ggtitle('Number of attacks per attack type') + facet_wrap(~ Var2) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
        axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))

### per ogni regione plotto i primi 5 tipi di attacco
attack_type <- data.frame(table(data$region_txt, data$attacktype1_txt))
attack_type <- attack_type %>%
  group_by(Var1) %>%
  arrange(desc(Freq)) %>%
  slice(1:6)

g <- ggplot(attack_type, aes(Var1,Freq, fill=Var2)) + geom_bar(stat="identity") +  ylab("Count") + xlab("Group name") +
  theme_bw() + ggtitle('Number of attacks per region and attack type') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
        axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
ggplotly(g)

## osservare se il numero di morti ? legato alla tipologia di attacco
### osserviamolo con i boxplot

#ggplot(data, aes(factor(attacktype1_txt), nkill)) + geom_boxplot(aes(fill = factor(attacktype1_txt)))

fit <- aov(nkill ~ attacktype1_txt, data=data)
summary(fit)  # pvalue basso ---> differenza significativa

TUKEY <- TukeyHSD(fit)
### osserviamo gli intervalli
TUKEY <- data.frame(TUKEY$attacktype1_txt)
TUKEY$pair <- rownames(TUKEY)

# Plot pairwise TukeyHSD comparisons and color by significance level
ggplot(TUKEY, aes(colour=cut(p.adj, c(0, 0.01, 0.05, 1), 
                             label=c("p<0.01","p<0.05","Non-Sig")))) +
  geom_hline(yintercept=0, lty="11", colour="grey30") +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size=1) +
  geom_point(aes(pair, diff),size=2) +
  labs(colour="") + coord_flip() + theme_bw()

### osservazioni
## l'attacco armato fa pi? morti delle bombe, degli omicidi, degli attacchi alle infrastrutture e di un assalto disarmato
## la presa di ostaggi con barricade incident fa pi? morti di un assalto armato, un omicidio, un attacco a infrastrutture e di esplosioni di bombe
## le bombe fanno pi? morti di un omicidio e di un attacco a infrastrutture

##########################################################
################# target dell'attacco ####################
##########################################################

target_type <- data.frame(table(data$targtype1_txt,data$continent))
target_type <- target_type[order(target_type$Freq,decreasing = T),]
ggplot(target_type, aes(reorder(Var1,Freq),Freq,fill=Var2)) + geom_bar(stat="identity") +  ylab("Count") + xlab("Group name") +
  theme_bw() + ggtitle('Number of attacks per target type') + facet_wrap(~ Var2) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
        axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))

### per ogni regione plotto i primi 5 tipi di attacco
target_type <- data.frame(table(data$region_txt, data$targtype1_txt))
target_type <- target_type %>%
  group_by(Var1) %>%
  arrange(desc(Freq)) %>%
  slice(1:6)

g <- ggplot(target_type, aes(Var1,Freq, fill=Var2)) + geom_bar(stat="identity") +  ylab("Count") + xlab("Group name") +
  theme_bw() + ggtitle('Number of attacks per region and target type') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
        axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
ggplotly(g)
p <- ggplotly(p=g)
htmlwidgets::saveWidget(as.widget(p), file = "target_per_region.html")


target_type <- data.frame(table(data$region_txt, data$continent, data$targtype1_txt))
g <- ggplot(target_type, aes(Var3,Freq, fill=Var1)) + geom_bar(stat="identity") +  ylab("Count") + xlab("Group name") +
  theme_bw() + ggtitle('Number of attacks per region and target type') + facet_wrap(~ Var2) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
        axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
ggplotly(g)
p <- ggplotly(p=g)
htmlwidgets::saveWidget(as.widget(p), file = "target_per_region_continent.html")

###################################################################?
################### WEAP TYPE ##############################
############################################################

weap_type <- data.frame(table(data$weaptype1_txt,data$continent))
weap_type <- weap_type[order(weap_type$Freq,decreasing = T),]
ggplot(weap_type, aes(reorder(Var1,Freq),Freq, fill=Var2)) + geom_bar(stat="identity") +  ylab("Count") + xlab("Group name") +
  theme_bw() + ggtitle('Number of attacks per weap type') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
        axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))

### per ogni regione plotto i primi 5 tipi di attacco
weap_type <- data.frame(table(data$region_txt, data$weaptype1_txt))
weap_type <- weap_type %>%
  group_by(Var1) %>%
  arrange(desc(Freq)) %>%
  slice(1:6)

g <- ggplot(weap_type, aes(Var1,Freq, fill=Var2)) + geom_bar(stat="identity") +  ylab("Count") + xlab("Group name") +
  theme_bw() + ggtitle('Number of attacks per region and weap type') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
        axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
ggplotly(g)

weap_type <- data.frame(table(data$region_txt, data$continent, data$weaptype1_txt))
g <- ggplot(weap_type, aes(Var3,Freq, fill=Var1)) + geom_bar(stat="identity") +  ylab("Count") + xlab("Group name") +
  theme_bw() + ggtitle('Number of attacks per region and weap type') + facet_wrap(~ Var2) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
        axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
ggplotly(g)

## osservare se il numero di morti ? legato alla tipologia di arma

fit <- aov(nkill ~ weaptype1_txt, data=data)
summary(fit)  # pvalue basso ---> differenza significativa

TUKEY <- TukeyHSD(fit)
### osserviamo gli intervalli
TUKEY <- data.frame(TUKEY$weaptype1_txt)
TUKEY$pair <- rownames(TUKEY)

# Plot pairwise TukeyHSD comparisons and color by significance level
ggplot(TUKEY, aes(colour=cut(p.adj, c(0, 0.01, 0.05, 1), 
                             label=c("p<0.01","p<0.05","Non-Sig")))) +
  geom_hline(yintercept=0, lty="11", colour="grey30") +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size=1) +
  geom_point(aes(pair, diff),size=2) +
  labs(colour="") + coord_flip() + theme_bw()

### direi che non c'? evidenza per dire che armi diverse diano diverso numero di morti

##################################################################################
################ mappa interattiva ###########################?
#################################################################?
library(ggmap)
library(mapproj)
library(rworldmap)

data$longitude <- as.numeric(gsub(',','.',data$longitude))
data$latitude <- as.numeric(gsub(',','.',data$latitude))
data$nkill <- as.numeric(data$nkill)

################### COMPLETO IL DATASET CON LATITUDINE E LONGITUDINE STIMATA
#city <- read.table("./dati/worldcitiespop.txt",
#                 header=T, sep = ",", na.strings = "NULL",stringsAsFactors = F)
#library(splitstackshape)

data$city[data$city=='Unknown'] <- NA
data$country_txt[data$country_txt=='Unknown'] <- NA

country <- read.csv("./Rome/dati/country.csv",
                    header=T, sep = ";", na.strings = "NULL",stringsAsFactors = F)
country$latitude1 <- as.numeric(unlist(sapply(country$latitude,function(x) if(substr(x, nchar(x)-3,nchar(x)-3)=='.'){paste(unlist(strsplit(x,''))[-gregexpr('\\.',x)[[1]][2]],sep='', collapse = "")} else{x})))
country$longitude1 <- as.numeric(unlist(sapply(country$longitude,function(x) if(substr(x, nchar(x)-3,nchar(x)-3)=='.'){paste(unlist(strsplit(x,''))[-gregexpr('\\.',x)[[1]][2]],sep='', collapse = "")} else{x})))
country$latitude1[is.na(country$latitude1)] <- as.numeric(country$latitude[is.na(country$latitude1)])
country$longitude1[is.na(country$longitude1)] <- as.numeric(country$longitude[is.na(country$longitude1)])

data$latitude[is.na(data$latitude)] <- unlist(sapply(tolower(data$country_txt[is.na(data$latitude)]), function(x) if(x %in% tolower(country$name)){country$latitude1[tolower(country$name)==x]} else {NA}))
data$longitude[is.na(data$longitude)] <- unlist(sapply(tolower(data$country_txt[is.na(data$longitude)]), function(x) if(x %in% tolower(country$name)){country$longitude1[tolower(country$name)==x]} else {NA}))

data$latitude[data$country=='Myanmar'] <- 21.9162
data$longitude[data$country=='Myanmar'] <- 95.9560

data$latitude[data$country=='West Bank and Gaza Strip'] <- 31.952162
data$longitude[data$country=='West Bank and Gaza Strip'] <- 35.23315400000001

data$latitude[data$country=='Macedonia'] <- 41.6086
data$longitude[data$country=='Macedonia'] <- 21.7453

data$latitude[data$country=='Democratic Republic of the Congo'] <- 4.0383
data$longitude[data$country=='Democratic Republic of the Congo'] <- 21.7587

data$latitude[data$country=='East Timor'] <- 8.8742
data$longitude[data$country=='East Timor'] <- 125.7275

data$latitude[data$country=='Ivory Coast'] <- 7.5400
data$longitude[data$country=='Ivory Coast'] <- 5.5471


save(data, file='./Rome/dati/data.RData')

library(ggmap)
map <- get_map(location = 'Europe', zoom = 3)
mapPoints <- ggmap(map) +geom_point(aes(x = longitude, y = latitude, size = nkill,fill='dodgerblue'), data = data, alpha = .5)
mapPoints 

##########################################
#mapDevice() 
library(rworldmap)
mapBubbles(dF=data, nameX ='longitude', nameY = 'latitude'
           ,nameZSize='nkill'
           ,nameZColour="region_txt"
           ,colourPalette="rainbow"
           ,oceanCol="lightblue"
           ,landCol="wheat", legendPos = NULL)

########################################
library(ggplot2)
library(maps)
library("magick")
library(animation)


for (i in min(year(data$timestamp)):max(year(data$timestamp))){
  name = paste(i,'plot.png',sep='')
  png(name)
  mdat <- map_data('world')
  
  #str(mdat)
  ggplot() + 
    geom_polygon(dat=mdat, aes(long, lat, group=group), fill="grey50") +
    geom_point(data=data[year(data$timestamp)==i,], 
               aes(x=longitude, y=latitude, size=nkill), col="red") + 
    scale_size(breaks = c(10,100,500,1000), trans = "identity", guide = "legend") +
    ggtitle(i) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
          axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
  dev.off()
}


########################################## INT
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
library(grid)

q1 <- qplot(as.factor(data$INT_LOG)) + ggtitle('Nationality of the perpetrator group vs location of the attack') + xlab('1=Logistically international, 0=Logistically domestic, -9=Unkwown')
q2 <- qplot(as.factor(data$INT_IDEO)) + ggtitle('Nationality of the perpetrator group vs nationality of the target') + xlab('1=Ideologically international, 0=Ideologically domestic, -9=Unkwown')
q3 <- qplot(as.factor(data$INT_MISC)) + ggtitle('Location of the attack vs nationality of the target') + xlab('1=miscellaneous international, 0=miscellaneous domestic, -9=Unkwown')
q4 <- qplot(as.factor(data$INT_ANY)) + ggtitle('If Any of the above resons') + xlab('1=the attack was international, 0 = the attack was domestic, -9=Unknown')

multiplot(q1,q2,q3,q4,cols=2)

grid.newpage()
grid.draw(rbind(ggplotGrob(q1), ggplotGrob(q2), ggplotGrob(q3), ggplotGrob(q4), size = "last"))

### variabili poco interessanti

####################### successo ##########
qplot(as.factor(data$succes)) + ggtitle('If the attack was a success')
ggplot(data, aes(x = factor(1), fill = factor(data$succes))) +
  geom_bar(width = 1) + coord_polar(theta = "y") + ggtitle('Success of the attack')

###################### property damage #############

ggplot(data, aes(x = factor(1), fill = factor(data$property))) +
  geom_bar(width = 1) + coord_polar(theta = "y") +  ggtitle('Attacks to property')

data$propvalue <- as.numeric(data$propvalue)
summary(data$propvalue)
quantile(data$propvalue[!(is.na(data$propvalue))])
### non ha molto senso il valore dei danni fatti


############################################################
############# ADDING INFORMATION #########################
###########################################################

country_info <- read.csv("./dati/countries.csv",
                         header=T, sep = ";", na.strings = "NULL",stringsAsFactors = F)

data <- merge(data,country_info[c('Country..en.','Population','Area','Government.form')],by.x='country_txt',by.y='Country..en.',all.x=T)

gov <- data.frame(table(data$Government.form))

g <- ggplot(gov, aes(reorder(Var1,Freq),Freq, fill=Var1)) + geom_bar(stat="identity") +  ylab("Count") + xlab("Group name") +
  theme_bw() + ggtitle('Number of attacks per weap type') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.text = element_text(colour = "azure4"),plot.title = element_text(colour = "azure4", face="bold" , hjust=0),
        axis.title.x = element_text(colour = "azure4"),axis.title.y = element_text(colour = "azure4"))
ggplotly(p=g)

######################################################
################ HEATMAPS ###########################?
######################################################
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
day <- data.frame(table(mymonths[month(data$timestamp)],weekdays(data$timestamp)))
day$Var1 <- ordered(day$Var1,levels=c("Jan","Feb","Mar",
                                      "Apr","May","Jun",
                                      "Jul","Aug","Sep",
                                      "Oct","Nov","Dec"))
day$Var2 <- ordered(day$Var2, levels= c('luned?','marted?','mercoled?','gioved?','venerd?','sabato','domenica'))
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')  
ggplot(day, aes(x = Var2, y = Var1, fill = Freq)) +  
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(100)) +
  ylab('Matrix columns') +
  xlab('Matrix rows') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

n_d <- ddply(data,.(mymonths[month(data$timestamp)],weekdays(data$timestamp)),
             summarize,
             nkill=sum(nkill[!(is.na(nkill))]))

names(n_d) <- c('Var1','Var2','nkill')

day <- merge(day,n_d,all.x=T)
day$ser <- day$nkill / day$Freq
day$Var1 <- ordered(day$Var1,levels=c("Jan","Feb","Mar",
                                      "Apr","May","Jun",
                                      "Jul","Aug","Sep",
                                      "Oct","Nov","Dec"))
day$Var2 <- ordered(day$Var2, levels= c('luned?','marted?','mercoled?','gioved?','venerd?','sabato','domenica'))

ggplot(day, aes(x = Var2, y = Var1, fill = ser)) +  
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(100)) +
  ylab('Matrix columns') +
  xlab('Matrix rows') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




