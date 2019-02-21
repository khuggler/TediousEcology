#' @title Stack and extract all spatial data to elk, coyotes, and mountain lions (RF Encounter)
#' @description Stack rasters and extract points data for elk, coyotes, and mountain lions
#' @param data data to be used to build RF model
#' @param ncpu number of cpus to parallelize on
#' @param withold percent of data (decimal format) to withold for training model (default 0.25)
#' @param trees number of trees (maximum) to attempt to split on
#' @param raspath path to raster stack
#' @param studypath path to study area polygon (where you want to define availability)
#' @param pathout path to where predicted RF map should be written
#' @return Returns a list object with RFData necessary to predict probably of use in RF models (elk, coyotes, and mountain lions)
#' @keywords elk, coyote, mountain lion, random forest, extract, raster, sample
#' @export

BuildRFModels<-function(data, ncpu, withold, trees, raspath, pathout){
  library(randomForest)
  library(snow)
  library(parallel)

  data$NLCD<-as.factor(data$NLCD)

  predictorNames <-c('Elevation', 'Slope', 'TPI', 'TRASP', 'TRI', 'PercentShrub', 'Roughness',
                     'PrimaryRd', 'SecondaryRd', 'TWI', 'NLCD', 'PercentSage', 'BigSage', 'SageHeight')

  pred.names<-c('Elevation', 'Slope', 'TPI', 'TRASP', 'TRI', 'PercentShrub', 'Roughness',
                'PrimaryRd', 'SecondaryRd', 'TWI', 'NLCD', 'PercentSage', 'BigSage', 'SageHeight')
  cbind(pred.names,predictorNames)

rasstack<-rasstack<-stack(raspath)
names(rasstack)<-c('Elevation', 'NLCD', 'Land', 'Roughness', 'Slope', 'TPI', 'TRASP', 'TRI', 'BigSage', 'PercentSage', 'PercentShrub', 'SageHeight', 'PrimaryRd', 'SecondaryRd', 'TWI')


  #################################################
  #### Create Training Data
  #########################################
  factor_Used<-as.factor(data$Used)
  fincpu<-ncpu
  cluster<-snow::makeCluster(fincpu)
  doParallel::registerDoParallel(cluster)

  p=withold
  n.train = as.integer(nrow(data)*p)
  indx<-sample(1:nrow(data), n.train)
  TrainData<-data[-indx,]
  TrainData<-TrainData[complete.cases(TrainData),]
  TestData<-data[indx,]
  TestData<-TestData[complete.cases(TestData),]
  snow::stopCluster(cluster)
  foreach::registerDoSEQ()


  cluster<-snow::makeCluster(fincpu)
  doParallel::registerDoParallel(cluster)

  TrainData$Used<-as.factor(TrainData$Used)
  TrainData<-TrainData[complete.cases(TrainData),]
  TrainData$Used<-make.names(TrainData$Used)
  system.time({LionRF_tune<-caret::train(x = TrainData[, pred.names],
                                         y = TrainData[, 'Used'],

                                         method = "rf",
                                         metric = "ROC",

                                         trControl = caret::trainControl(method = "cv",
                                                                         classProbs = TRUE,
                                                                         summaryFunction = caret::twoClassSummary),
                                         tuneLength = trees, verbose = T)})

  snow::stopCluster(cluster)
  foreach::registerDoSEQ()


  TestPred<-predict(LionRF_tune, TestData) ## Okay, model is good
  TestData$Used<-as.factor(make.names(TestData$Used))
  conf<-caret::confusionMatrix(TestPred, TestData$Used)

  library(pROC)
  result.predicted.prob <- predict(LionRF_tune, TestData, type="prob") # Prediction
  result.roc <- roc(TestData$Used, result.predicted.prob[,2]) # Draw ROC curve.
  plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")
  result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
  print(result.coords)

  LionRF_tune
  x<-LionRF_tune['bestTune']
  m<-as.numeric(x[[1]])

  data<-data[complete.cases(data),]
  factor_Used<-as.factor(data$Used)
  thresh<-as.numeric(result.coords[1])
  LionRF_final<-randomForest(data[,pred.names], factor_Used, mtry = m, cutoff = c(thresh, 1-thresh)) ## best mtry from tuning is 10
  plot(LionRF_final, bty = "l", main = "Predictive Error")

  varImpPlot(LionRF_final, bty = "l", main = "Lion Random Forest")


  LionMap<-raster::predict(rasstack, model = LionRF_final, progress = "text", type = "prob", index =2)

  spplot(LionMap)

  writeRaster(LionMap, pathout, format = "HFA", overwrite = TRUE)
  return(conf)

}
