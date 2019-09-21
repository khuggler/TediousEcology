#' @title BuildRF Model with locations of mountain lion kill sites (summer and winter models)
#' @description Build RF Model with locations of mountain lion kill sites
#' @param pred.data path of data to be used to build RF model
#' @param ncpu number of cpus to parallelize on
#' @param withold percent of data (decimal format) to withold for training model (default 0.25)
#' @param method method used for partitioning and training model ("bootstrap" or "repeatedcv")
#' @param raspath path to raster stack
#' @param studypath path to study area polygon (where you want to define availability)
#' @param pathout path to where predicted RF map should be written
#' @return Returns a list object with RFData necessary to predict probably of use in RF models (elk, coyotes, and mountain lions)
#' @keywords elk, coyote, mountain lion, random forest, extract, raster, sample
#' @export

BuildPredRiskModel<-function(pred.data, ncpu, withold,method, combine.data){
  library(raster)
  library(rgdal)
  library(randomForest)
  library(doParallel)
  library(pROC)

  pr<-read.csv(pred.data,stringsAsFactors = F) ### Extracted data for Used and aVailable points
  pr$RasterStack_NLCD_11_30<-as.factor(pr$RasterStack_NLCD_11_30)
  pr$NewKill<-ifelse(pr$Kill == 1 & pr$Season == "Winter", 2, ifelse(pr$Kill == 1 & pr$Season == "Summer", 3, pr$Kill))
  pr<-pr[complete.cases(pr),]

  SummerAll<-pr[pr$Season == "Summer",]
  WinterAll<-pr[pr$Season == "Winter",]

  pred.names<-(names(pr[,c(9:15, 17:23)]))

  #### Summer and Winter Models Separately ####
  sum<-TediousEcology::boot.fun(data = SummerAll, prop = 0.8, n.boot = 500, samplesize = c(50, 10), mtry = 4, cutoff = 0.75, pred.names = pred.names, cat.column = "Kill")

  sum<-sum[[1]]
  sumagg<-aggregate(sum$KillPred, by = list(sum$ID, sum$Kill), FUN = mean, na.rm =TRUE)
  sumagg$SD<-aggregate(sum$KillPred, by = list(sum$ID, sum$Kill), FUN = sd, na.rm = TRUE)[,3]
  sumagg$SE<-aggregate(sum$KillPred, by = list(sum$ID, sum$Kill), FUN = function(x) sd(x)/sqrt(length(x)))[,3]
  names(sumagg)<-c('ID', 'Kill','KillPrediction', 'SD', 'SE')


  library(easyGgplot2)
  library(ggpubr)
  print(sumplot<-ggplot2.violinplot(data = sumagg, xName = 'Kill', yName = 'KillPrediction',addDot = TRUE, dotSize = 0.5, dotPosition = "center"))


  print(sumplot<-sumplot + theme(panel.grid.major = element_line(linetype = "blank"),
                             panel.grid.minor = element_line(linetype = "blank"),
                             panel.background = element_rect(fill = NA)) +labs(title = "Summer"))



  ### Winter ###
  win<-TediousEcology::boot.fun(data = WinterAll, prop = 0.8, n.boot = 500, mtry = 4, samplesize = c(50,10), cutoff = 0.75, pred.names = pred.names, cat.column = "Kill")
  win<-win[[1]]
  winagg<-aggregate(win$KillPred, by = list(win$ID, win$Kill), FUN = mean, na.rm =TRUE)
  winagg$SD<-aggregate(win$KillPred, by = list(win$ID, win$Kill), FUN = sd, na.rm = TRUE)[,3]
  winagg$SE<-aggregate(win$KillPred, by = list(win$ID, win$Kill), FUN = function(x) sd(x)/sqrt(length(x)))[,3]
  names(winagg)<-c('ID', 'Kill','KillPrediction', 'SD', 'SE')


  print(winplot<-ggplot2.violinplot(data = winagg, xName = 'Kill', yName = 'KillPrediction',addDot = TRUE, dotSize = 0.5, dotPosition = "center"))


  print(winplot<-winplot + theme(panel.grid.major = element_line(linetype = "blank"),
                             panel.grid.minor = element_line(linetype = "blank"),
                             panel.background = element_rect(fill = NA)) +labs(title = "Winter"))

  print(season.plot<-ggarrange(sumplot, winplot))

##### Combine Winter and Summer ######
  dev.off()
  all.season<-TediousEcology::boot.fun(data = pr, prop = 0.8, n.boot = 500, mtry = 4, samplesize = c(50,10), cutoff = 0.75, pred.names = pred.names, cat.column = "Kill")
  all.season<-all.season[[1]]
  all.agg<-aggregate(all.season$KillPred, by = list(all.season$ID, all.season$Kill, all.season$Season), FUN = mean, na.rm =TRUE)
  all.agg$SD<-aggregate(all.season$KillPred, by = list(all.season$ID, all.season$Kill, all.season$Season), FUN = sd, na.rm = TRUE)[,3]
  all.agg$SE<-aggregate(all.season$KillPred, by = list(all.season$ID, all.season$Kill, all.season$Season), FUN = function(x) sd(x)/sqrt(length(x)))[,3]
  names(all.agg)<-c('ID', 'Kill','Season', 'KillPrediction', 'SD', 'SE')

print(vplot<-ggplot2.violinplot(data = all.agg, xName = 'Kill', yName = 'KillPrediction',addDot = TRUE, dotSize = 0.5, dotPosition = "center",
groupName = 'Season', groupColors = c('firebrick3', 'dodgerblue4')))

print(vplot<-vplot + theme(panel.grid.major = element_line(linetype = "blank"),
                       panel.grid.minor = element_line(linetype = "blank"),
                       panel.background = element_rect(fill = NA)) +labs(title = "Distribution of Kill Site Predictions"))

####### Test for differences in Winter and Summer ###########
dev.off()

new.dat<-pr[pr$NewKill >0, ]
winsum<-TediousEcology::boot.fun(data = new.dat, prop = 0.8, n.boot = 500, samplesize = c(1,1), mtry = 4, cutoff = 0.5, pred.names = pred.names, cat.column = "NewKill")
winsum<-winsum[[1]]
winsumagg<-aggregate(winsum$KillPred, by = list(winsum$ID, winsum$NewKill), FUN = mean, na.rm =TRUE)
winsumagg$SD<-aggregate(winsum$KillPred, by = list(winsum$ID, winsum$NewKill), FUN = sd, na.rm = TRUE)[,3]
winsumagg$SE<-aggregate(winsum$KillPred, by = list(winsum$ID, winsum$NewKill), FUN = function(x) sd(x)/sqrt(length(x)))[,3]
names(winsumagg)<-c('ID', 'Kill','KillPrediction', 'SD', 'SE')


print(winsumplot<-ggplot2.violinplot(data = winsumagg, xName = 'Kill', yName = 'KillPrediction',addDot = TRUE, dotSize = 0.5, dotPosition = "center"))


print(winsumplot<-winsumplot + theme(panel.grid.major = element_line(linetype = "blank"),
                               panel.grid.minor = element_line(linetype = "blank"),
                               panel.background = element_rect(fill = NA)) +labs(title = "Distribution of Winter and Summer Kill Site"))

######### Investigate how model can be improved with combined winter and summer data #############
dev.off()
all.season<-TediousEcology::boot.fun(data = pr, prop = 0.8, n.boot = 500, mtry = 4, samplesize = c(50,10), cutoff = 0.75, pred.names = pred.names, cat.column = "Kill")
all.season<-all.season[[1]]
all.agg<-aggregate(all.season$KillPred, by = list(all.season$ID, all.season$Kill), FUN = mean, na.rm =TRUE)
all.agg$SD<-aggregate(all.season$KillPred, by = list(all.season$ID, all.season$Kill), FUN = sd, na.rm = TRUE)[,3]
all.agg$SE<-aggregate(all.season$KillPred, by = list(all.season$ID, all.season$Kill), FUN = function(x) sd(x)/sqrt(length(x)))[,3]
names(all.agg)<-c('ID', 'Kill','KillPrediction', 'SD', 'SE')

print(vplot<-ggplot2.violinplot(data = all.agg, xName = 'Kill', yName = 'KillPrediction',addDot = TRUE, dotSize = 1, dotPosition = "jitter"))


print(vplot<-vplot + theme(panel.grid.major = element_line(linetype = "blank"),
                           panel.grid.minor = element_line(linetype = "blank"),
                           panel.background = element_rect(fill = NA)) +labs(title = "Distribution of Kill Site Predictions"))

####### Pull out data where the predictions are "good" ######
whole.agg<-all.agg
whole.agg$Keep<-ifelse(whole.agg$Kill == 1 & whole.agg$KillPrediction < 0.2 | whole.agg$Kill == 0 & whole.agg$KillPrediction > 0.8, 0, 1)

keep<-whole.agg[whole.agg$Keep ==1,]
nokeep<-whole.agg[whole.agg$Keep == 0,]

unikeep<-paste(keep$ID, keep$Kill, sep = "_")
uninokeep<-paste(nokeep$ID, nokeep$Kill, sep = "_")

pr$NewID<-paste(pr$ID, pr$Kill, sep = "_")

newdata<-pr[pr$NewID %in% unikeep,]
otherdata<-pr[pr$NewID %in% uninokeep,]

all.season.2<-TediousEcology::boot.fun(data = newdata, prop = 0.8, n.boot = 500, mtry = 4, samplesize = c(50,10), cutoff = 0.75, pred.names = pred.names, cat.column = "Kill")
all.season.2<-all.season.2[[1]]
all.agg<-aggregate(all.season.2$KillPred, by = list(all.season.2$ID, all.season.2$Kill), FUN = mean, na.rm =TRUE)
all.agg$SD<-aggregate(all.season.2$KillPred, by = list(all.season.2$ID, all.season.2$Kill), FUN = sd, na.rm = TRUE)[,3]
all.agg$SE<-aggregate(all.season.2$KillPred, by = list(all.season.2$ID, all.season.2$Kill), FUN = function(x) sd(x)/sqrt(length(x)))[,3]
names(all.agg)<-c('ID', 'Kill','KillPrediction', 'SD', 'SE')

print(vplot<-ggplot2.violinplot(data = all.agg, xName = 'Kill', yName = 'KillPrediction',addDot = TRUE, dotSize = 1, dotPosition = "jitter"))


print(vplot<-vplot + theme(panel.grid.major = element_line(linetype = "blank"),
                           panel.grid.minor = element_line(linetype = "blank"),
                           panel.background = element_rect(fill = NA)) +labs(title = "Distribution of Kill Site Predictions"))

##### It's clear that winter and summer data can't be differentiated, so we can combine ######
######################## Run a Model with just geomorphic variables ##############################

geo.pred.names<-(names(pr[,c(9,15,19,21:23)]))

geo.model<-TediousEcology::boot.fun(data = newdata, prop = 0.8, n.boot = 500, mtry = 4, samplesize = c(50,10), cutoff = 0.75, pred.names = geo.pred.names, cat.column = "Kill")
geo.model<-geo.model[[1]]
geo.agg<-aggregate(geo.model$KillPred, by = list(geo.model$ID, geo.model$Kill), FUN = mean, na.rm =TRUE)
geo.agg$SD<-aggregate(geo.model$KillPred, by = list(geo.model$ID, geo.model$Kill), FUN = sd, na.rm = TRUE)[,3]
geo.agg$SE<-aggregate(geo.model$KillPred, by = list(geo.model$ID, geo.model$Kill), FUN = function(x) sd(x)/sqrt(length(x)))[,3]
names(geo.agg)<-c('ID', 'Kill','KillPrediction', 'SD', 'SE')

print(vplot<-ggplot2.violinplot(data = geo.agg, xName = 'Kill', yName = 'KillPrediction',addDot = TRUE, dotSize = 1, dotPosition = "jitter"))


print(vplot<-vplot + theme(panel.grid.major = element_line(linetype = "blank"),
                           panel.grid.minor = element_line(linetype = "blank"),
                           panel.background = element_rect(fill = NA)) +labs(title = "Distribution of Kill Site Predictions"))


##### Well, this doesn't make accuracy any better, seems as though removing outliers, and keep all variables is the best way forward ####

predrisk<-randomForest(newdata[, pred.names], as.factor(newdata[,'Kill']), sampsize = c(317,56), mtry = 4, cutoff = c(0.75, 1-0.75))

files<-unzip(raspath, files = NULL)
files<-grep(".img$", files, value = TRUE)

r<-list()
for(i in 1:length(files)){
  tempras<-raster(files[i])
  r[[i]]<-assign(names(tempras), tempras)

stack<-lapply(r, stack)
rasstack<-stack(stack)
}


  predmap<-raster::predict(rasstack, predrisk, progress= "text", type ="prob", index = 2)
  writeRaster(predmap, 'C:/Users/khuggler/Box Sync/DEER/GradStudentWork/Huggler/Chapter1/Analyses/Data/PredationRiskLayer/PredRiskSummer.img', format = "HFA",
              overwrite = TRUE)
  varImpPlot(predrisk)

}







