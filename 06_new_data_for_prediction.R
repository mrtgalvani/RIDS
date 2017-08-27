setwd('C:/Users/m.galvani/Desktop/RIDS/Terrorism/Script')
rm(list=ls())

library(lubridate)
library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations
library(PRROC) # for Precision-Recall curve calculations
library(ff)
library(ROCR)
#library(xts)

source("./Rome//function.R")
source("./Rome/forecast.R")  

#load('./Rome/dati/data.RData')
load('./Rome/dati/data_cluster_europe.RData')

#data <- data[data$region_txt=='Europe - Western Europe',]

#data <- data[data$continent=='America',]
data$country_txt <- tolower(data$country_txt)
data$week <- round_date(data$timestamp, unit="week")

#month <- data.frame(table(data$provstate,format(as.Date(data$timestamp), "%Y-%m")))
#month <- data.frame(table(data$country_txt,format(as.Date(data$timestamp), "%Y-%m")))
month <- data.frame(table(data$country_txt,data$week))

names(month) <- c('country','timestamp','nattack')
month$timestamp <- as.character(month$timestamp)
month$timestamp <- paste0(month$timestamp,'-01')
month$timestamp <- as.POSIXct(month$timestamp,format='%Y-%m-%d')
month$year <- year(month$timestamp)
#month$timestamp <- month$timestamp %m-% months(1)

############# count previous attacks ###############
month$prev_attack_6M <- 0
for(i in 1:nrow(month)){
  month$prev_attack_6M[i]<- sum(month$nattack[month$country==month$country[i] & month$timestamp>=month$timestamp[i] %m-% months(6) & month$timestamp<month$timestamp[i]])
}
# month$flg_prev_attack_6M <- ifelse(month$prev_attack_6M>0,1,0)
# 
# month$prev_attack_3M <- 0
# for(i in 1:nrow(month)){
#   month$prev_attack_3M[i]<- sum(month$nattack[month$country==month$country[i] & month$timestamp>=month$timestamp[i] %m-% months(3) & month$timestamp<month$timestamp[i]])
# }
# month$flg_prev_attack_3M <- ifelse(month$prev_attack_3M>0,1,0)

month$prev_oth_attack_6M <- 0
for(i in 1:nrow(month)){
  month$prev_oth_attack_6M[i]<- sum(month$nattack[month$country!=month$country[i] & month$timestamp>=month$timestamp[i] %m-% months(6) & month$timestamp<month$timestamp[i]])
}
# month$flg_oth_prev_attack_6M <- ifelse(month$prev_oth_attack_6M>0,1,0)
# 
# month$prev_oth_attack_3M <- 0
# for(i in 1:nrow(month)){
#   month$prev_oth_attack_3M[i]<- sum(month$nattack[month$country!=month$country[i] & month$timestamp>=month$timestamp[i] %m-% months(3) & month$timestamp<month$timestamp[i]])
# }
# month$flg_oth_prev_attack_3M <- ifelse(month$prev_oth_attack_3M>0,1,0)

############ Next attack ######################

# month$next_attack <- 0
# for(i in 1:nrow(month)){
#    #month$next_attack[i]<- sum(month$nattack[month$country==month$country[i] & month$timestamp<=month$timestamp[i] %m+% months(1) & month$timestamp>month$timestamp[i]])
#    month$next_attack[i]<- sum(month$nattack[month$country==month$country[i] & month$timestamp<=month$timestamp[i] + 7*24*60*60 & month$timestamp>month$timestamp[i]])
#    
#    }
#month$flg_next_attack <- ifelse(month$next_attack>0,1,0)

month$flg_next_attack <- ifelse(month$nattack>0,1,0)

summary(factor(month$flg_next_attack))

#save(month,file='./../dati/attack_per_provstate.RData')

###################################################
################# country #########################
###################################################

# country <- read.csv('./../dati/countries.csv', sep=';')
# country <- country[country$Continent=='Europe',]
# country$Country..en. <- tolower(country$Country..en.)
# # 
# month <- merge(month,country[c('Country..en.','Population','Area','Coastline','Government.form')],
#                by.x='country',by.y='Country..en.')#,all.x=T)
# 
# ### OSS: manca cipro
# 
# ###################################################
# ######### MAJOR EPISODES OF POLITICAL VIOLENCE ####
# ##################################################
# 
# # MEPV <- read.csv('./../dati/MEPV2012ex.csv', sep=';') # solo fino 2012
# # 
## month$year <- year(month$timestamp)
# # MEPV$COUNTRY <- tolower(MEPV$COUNTRY)
# # 
# # MEPV <- MEPV[-which(names(MEPV)%in% c('SCODE','CCODE','nBORDER','REGION','nREGION','NCIVLIST','NINTLIST','NACLIST'))]
# # 
# # month <- merge(month,MEPV,by.x=c('country','year'),by.y=c('COUNTRY','YEAR'))#,all.x=T)
# 
# ### mancano ICELAND e MALTA
# ###################################################
p4v2015 <- read.csv('./Rome/dati/p4v2015.csv', sep=';')

p4v2015$country <- tolower(p4v2015$country)
p4v2015$xrreg[p4v2015$xrreg==1] <- 'UNREGULATED'
p4v2015$xrreg[p4v2015$xrreg==2] <- 'TRANSITIONAL'
p4v2015$xrreg[p4v2015$xrreg==3] <- 'REGULATED'
p4v2015$xrreg <- as.factor(p4v2015$xrreg)

p4v2015$xrcomp[p4v2015$xrcomp==1] <- 'SELECTION'
p4v2015$xrcomp[p4v2015$xrcomp==2] <- 'TRANSITIONAL'
p4v2015$xrcomp[p4v2015$xrcomp==3] <- 'ELECTION'
p4v2015$xrcomp <- as.factor(p4v2015$xrcomp)

p4v2015$xropen[p4v2015$xropen==1] <- 'CLOSED'
p4v2015$xropen[p4v2015$xropen==2] <- 'DESIGNATION'
p4v2015$xropen[p4v2015$xropen==3] <- 'ELECTION'
p4v2015$xropen[p4v2015$xropen==4] <- 'OPEN'
p4v2015$xropen <- as.factor(p4v2015$xropen)


### XCONST: al crescere aumentano le limitazione goverantive

p4v2015$parreg[p4v2015$parreg==1] <- 'UNREGULATED'
p4v2015$parreg[p4v2015$parreg==2] <- 'MULTIPLE_IDENTITY'
p4v2015$parreg[p4v2015$parreg==3] <- 'SECTARIAN'
p4v2015$parreg[p4v2015$parreg==4] <- 'RESTRICTED'
p4v2015$parreg[p4v2015$parreg==5] <- 'REGULATED'
p4v2015$parreg <- as.factor(p4v2015$parreg)


p4v2015$parcomp[p4v2015$parcomp==1] <- 'REPRESSED'
p4v2015$parcomp[p4v2015$parcomp==2] <- 'SUPPRESSED'
p4v2015$parcomp[p4v2015$parcomp==3] <- 'FRACTIONAL'
p4v2015$parcomp[p4v2015$parcomp==4] <- 'TRANSITIONAL'
p4v2015$parcomp[p4v2015$parcomp==5] <- 'COMPETITIVE'
p4v2015$parcomp <- as.factor(p4v2015$parcomp)

month$country <- tolower(month$country)
month <- merge(month,
               p4v2015[c('country','year','polity2','fragment','durable')],#,'xrreg','parcomp','parreg','xropen','xrcomp')],
               by.x=c('country','year'),by.y=c('country','year'))#,all.x=T)

# mancano iceland e malta

####################################################
SFIv2015 <- read.csv('./Rome/dati/SFIv2015.csv', sep=';')

SFIv2015$country <- tolower(SFIv2015$country)

SFIv2015 <- SFIv2015[-which(names(SFIv2015) %in% c('region','scode','soceff','socleg','ecoeff'))]

month <- merge(month,SFIv2015,by.x=c('country','year'),by.y=c('country','year'))#,all.x=T)
# 
# #mancano iceland e malta

####################################################àà
# 
# gap <-  read.csv('./../dati/gapminder.csv', sep=',')
# gap$country <- tolower(gap$country)
# 
# gap <- gap[-which(names(gap) %in% c('alcconsumption','breastcancerper100th','co2emissions','femaleemployrate','hivrate','oilperperson','relectricperperson','suicideper100th'))]
# 
# month <- merge(month,gap)

# 
# gap <-  read.csv('./../dati/NMC_5_0.csv', sep=',')
# 
# gap <- gap[-which(names(gap) %in% c('ccode','version'))]
# 
# country <- read.csv('./../dati/countrycodes.csv', sep=';')
# country$Country..en. <- tolower(country$Country..en.)
# 
# month <- merge(month,country[c('Country..en.','ISO.3166.1.alpha3','ISO.3166.1.alpha2')],by.x='country',by.y='Country..en.')
# 
# month <- merge(month,gap,by.x=c('ISO.3166.1.alpha3','year'),by.y=c('stateabb','year'))

####################################################
############### LOGISTIC REGRESSION ################
####################################################

# provare a fare ciclo for sugli hotspot con modello di prevision con provstate

month_logistic <- month[-which(names(month) %in% c('year','timestamp','next_attack','flg_prev_attack_6M',
                                                   'prev_attack_3M','flg_prev_attack_3M','flg_oth_prev_attack_3M','flg_oth_prev_attack_6M',
                                                   'prev_oth_attack_3M','nattack'))]

train <- month_logistic[month$timestamp<=as.POSIXct('2014-12-31'),]
test <- month_logistic[month$timestamp>as.POSIXct('2014-12-31'),]

model <- glm(flg_next_attack ~.,family=binomial(link='logit'),data=train)
summary(model)

fitted.results <- predict(model,newdata=test[-which(names(test)=='flg_next_attack')],type='response')
pred <- ifelse(fitted.results > 0.3,1,0)

misClasificError <- mean(pred != test$flg_next_attack)
print(paste('Accuracy',1-misClasificError))

mc <- table(pred, test$flg_next_attack)
mc

library(ROCR)
pr <- prediction(pred, test$flg_next_attack)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

###############################################################
######################## UNBALANCED ###########################
###############################################################

library('caret')
month_mod <- month[-which(names(month) %in% c('year','timestamp','next_attack','flg_prev_attack_6M',
                                              'prev_attack_3M','flg_prev_attack_3M','flg_oth_prev_attack_3M','flg_oth_prev_attack_6M',
                                              'prev_oth_attack_3M','nattack'))]
month_mod$country <- gsub(' ','_',month_mod$country)

index <- createDataPartition(month_mod$flg_next_attack, p=0.75, list=FALSE)
train <- month_mod[ index,]
test <- month_mod[-index,]

ctrl <- trainControl(
  method = "cv",
  number = 5,
  #repeats = 4,
  summaryFunction = twoClassSummary,
  classProbs = TRUE)

train$flg_next_attack <- ifelse(train$flg_next_attack==0,'no','si')
test$flg_next_attack <- ifelse(test$flg_next_attack==0,'no','si')

################################à
##### GBM CLASSICO ##########à
#############################
origin_fit <- train(flg_next_attack ~ .,
                    data = train,
                    method = "gbm",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl)

#### calcolo il valore auc

# ROC: vari positivi vs falsi positivi all'aumentare del thereshold
test_roc <- function(model, data) {
  pred <- predict(model,test,type='prob')['si']
  roc(test$flg_next_attack[],
      pred$si)
}

test_pr <- function(model, data) {
  pred <- predict(model,test,type='prob')['si']
  pred <- prediction(pred$si,test$flg_next_attack)
  performance(pred, "prec", "rec")
}

origin_fit %>%
  test_roc(data = test) %>%
  auc()
# AUC = rea sotto la curva ROC: measure of how well a parameter can distinguish between two diagnostic groups
################################à
##### GBM PESATO ##########à
############################# 

model_weights <- ifelse(train$flg_next_attack == "no",
                        (1/table(train$flg_next_attack)[1]) * 0.5,
                        (1/table(train$flg_next_attack)[2]) * 0.5)
#Use the same seed to ensure same cross-validation splits
ctrl$seeds <- origin_fit$control$seeds

weighted_fit <- train(flg_next_attack ~ .,
                      data = train,
                      method = "gbm",
                      verbose = FALSE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = ctrl)

################################à
##### GBM DOWN ##########à
############################# 
ctrl$sampling <- "down"

down_fit <- train(flg_next_attack ~ .,
                  data = train,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

################################à
##### GBM SMOTE ##########à
############################# 
ctrl$sampling <- "smote"

smote_fit <- train(flg_next_attack ~ .,
                   data = train,
                   method = "gbm",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl)
########################################
##### Bayesian Linear Model wheighted ###
########################################
ctrl$sampling <- NULL

bay_fit <- train(flg_next_attack ~ .,
                 data = train,
                 method = "bayesglm",
                 
                 weights = model_weights,
                 metric = "ROC",
                 trControl = ctrl)

###################################
##### Bayesian Linear Model down ###
###################################
ctrl$sampling <- "down"

bay_fit_down <- train(flg_next_attack ~ .,
                      data = train,
                      method = "bayesglm",
                      
                      metric = "ROC",
                      trControl = ctrl)
########################################
##### Naural Network wheighted ###
########################################
# ctrl$sampling <- NULL
# 
# nn_fit <- train(flg_next_attack ~ .,
#                  data = train,
#                  method = "nnet",
#                  
#                  weights = model_weights,
#                  metric = "ROC",
#                  trControl = ctrl)
# 
# ###################################
# ##### Neural Network down ###
# ###################################
# ctrl$sampling <- "down"
# 
# nn_fit_down <- train(flg_next_attack ~ .,
#                       data = train,
#                       method = "nnet",
#                       
#                       metric = "ROC",
#                       trControl = ctrl)
########################################
##### General Linear Model wheighted ###
########################################
ctrl$sampling <- NULL

glm_fit <- train(flg_next_attack ~ .,
                 data = train,
                 method = "glmboost",
                 
                 weights = model_weights,
                 metric = "ROC",
                 trControl = ctrl)

###################################
##### General Linear Model down ###
###################################
ctrl$sampling <- "down"

glm_fit_down <- train(flg_next_attack ~ .,
                      data = train,
                      method = "glmboost",
                      
                      metric = "ROC",
                      trControl = ctrl)

#############################
##### knn weighted ##########
############################# 
ctrl$sampling <- NULL

knn_fit <- train(flg_next_attack ~ .,
                 data = train,
                 method = "knn",
                 metric = "ROC",
                 weights = model_weights,
                 trControl = ctrl)
#############################
##### knn down ##########
############################# 
ctrl$sampling <- "down"
knn_fit_down <- train(flg_next_attack ~ .,
                      data = train,
                      method = "knn",
                      metric = "ROC",
                      trControl = ctrl)

################################à
##### Random Forest ##########à
############################# 
ctrl$sampling <- "down"

rf_fit <- train(flg_next_attack ~ .,
                data = train,
                method = "ranger",
                #weights = model_weights,                   
                metric = "ROC",
                trControl = ctrl)

####################
##### EVTREE #######
####################
ctrl$sampling <- "down"

evtree_fit <- train(flg_next_attack ~ .,
                    data = train,
                    method = "evtree",
                    #weights = model_weights,  
                    metric = "ROC",
                    trControl = ctrl)


############################################
##### CONFRONTO I MODELLI  ##################
########################################

model_list <- list(#gbm_original = origin_fit,
  gbm_weighted = weighted_fit,
  gbm_down = down_fit,
  #gbm_SMOTE = smote_fit,
  glm_weighted = glm_fit,
  glm_down = glm_fit_down,
  knn_weighted = knn_fit,
  knn_down = knn_fit_down,
  rf_down =rf_fit,
  evtree_down = evtree_fit,
  bayes_down = bay_fit_down,
  bayes_weighted = bay_fit)

model_list_roc <- model_list %>%
  map(test_roc, data = test)

check <- data.frame(model_list_roc %>%
                      map(auc))
write.csv(check, file='auc.csv')

model_list_pr <- model_list %>%
  map(test_pr, data = test)

# seleziono i modelli con auc maggiore per l'averaging

##############################################
############## ENSAMBLE #####################à
############################################
test$pred_rf_prob<-predict(object = bay_fit_down,test,type='prob')
test$pred_knn_prob<-predict(object = rf_fit,test,type='prob')
test$pred_lr_prob<-predict(object = down_fit,test,type='prob')

######## AVERAGING ###########
test$pred_avg<- (test$pred_rf_prob$si+test$pred_knn_prob$si+test$pred_lr_prob$si)/3

#Splitting into binary classes at 0.5
#test$pred_avg<-as.factor(ifelse(test$pred_avg>0.5,'si','no'))

####### MAJORITY VOTE ########
# test$pred_lr<-predict(object = glm_fit,test)
# test$pred_knn<-predict(object = glm_fit_down,test)
# test$pred_rf<-predict(object = down_fit,test)
# 
# test$pred_majority<-as.factor(ifelse(test$pred_rf=='si' & test$pred_knn=='si','si',ifelse(test$pred_rf=='Y' & test$pred_lr=='si','si',ifelse(test$pred_knn=='si' & test$pred_lr=='si','si','no'))))

roc_avg <- roc(test$flg_next_attack[],test$pred_avg[])
model_list_roc$avg_model <- roc_avg

pr_avg <- prediction(test$pred_avg,test$flg_next_attack)
pr_avg <- performance(pr_avg, "prec", "rec")
model_list_pr$avg_model <- pr_avg

####################################################
#################### ROC ##########################
#################################################

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = c(names(model_list),'avg_model')[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7",'dodgerblue','red','blue','deeppink','chocolate',
                'gray','magenta','purple','yellow')

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) + xlab('False Positive Rate') + ylab('True Positive Rate') +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18) + ggtitle('ROC curve')

####################################################
#################### PR ##########################
#################################################


results_list_pr <- list(NA)
num_mod <- 1

for(the_pr in model_list_pr){
  
  results_list_pr[[num_mod]] <- 
    data_frame(tpr = unlist(the_pr@y.values),
               fpr = unlist(the_pr@x.values),
               model = c(names(model_list),'avg_model')[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_pr <- bind_rows(results_list_pr)

# Plot ROC curve for all 5 models

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_pr) +
  geom_line(aes(color = model), size = 1) + xlab('Recall') + ylab('Precision') +
  scale_color_manual(values = custom_col) +
  theme_bw(base_size = 18) + ggtitle('Precision Recall Curve')

############################################
############### GRAFICI ####################
############################################


### stati rischiosi con prob ultimo mese ###
month$country <- gsub(' ','_',month$country)
month$pred_rf_prob<-predict(object = bay_fit_down,month,type='prob')
month$pred_knn_prob<-predict(object = rf_fit,month,type='prob')
month$pred_lr_prob<-predict(object = down_fit,month,type='prob')

######## AVERAGING ###########
month$prob <- (month$pred_rf_prob$si+month$pred_knn_prob$si+month$pred_lr_prob$si)/3

state <- month[month$timestamp>=as.POSIXct('2015-12-26'),]

country <- read.csv('./Rome/dati/countrycodes.csv', sep=';')
country$Country..en. <- tolower(country$Country..en.)

state <- merge(state,country[c('Country..en.','ISO.3166.1.alpha3','ISO.3166.1.alpha2')],by.x='country',by.y='Country..en.')

library(dplyr)
library(maptools)
library(ggplot2)
library(plotly)
df <- state
df <- df[-which(names(df) %in% c('pred_rf_prob','pred_lr_prob','pred_knn_prob'))]

g <- list(
  scope = 'europe')

plot_geo(df) %>%
  add_trace(
    z = ~prob, locations = ~ISO.3166.1.alpha3,
    color = ~prob, colors = 'Oranges'
  ) %>%
  colorbar(title = "") %>%
  layout(geo = g
  )

#### oppure
library(ggplot2)
library(grid)
library(rworldmap)

worldMap <- getMap()

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


# Member States of the European Union
europeanUnion <- unique(unlist(sapply(as.character(state$country),simpleCap)))
indEU <- which(worldMap$NAME%in%europeanUnion)

europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

value <- state$prob

europeanUnionTable <- data.frame(country = europeanUnion, value = value)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]

# Plot the map
P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

P <- P + scale_fill_gradient(name = "Growth Rate", low = "white", high = "red", na.value = "grey50")


P <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))

P

################# per provstate ###################
