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
  pr$NLCD<-as.factor(pr$NLCD)

  SummerAll<-pr[pr$Season == "Summer",]
  WinterAll<-pr[pr$Season == "Winter",]

  predictorNames <-c('Elevation', 'Slope', 'TPI', 'TRASP', 'TRI', 'PercentShrub', 'Roughness',
                      'SecondaryRd', 'NLCD', 'PercentSage', 'BigSage', 'SageHeight')

  pred.names<-c('Elevation', 'Slope', 'TPI', 'TRASP', 'TRI', 'PercentShrub', 'Roughness',
                'SecondaryRd', 'NLCD', 'PercentSage', 'BigSage', 'SageHeight')
  cbind(pred.names,predictorNames)

  ################### Create training and test data ###############
  p=withold
  TrainUse<-SummerAll[SummerAll$Kill == 1,]
  n.train = as.integer(nrow(TrainUse)*p)
  Usedindx<-sample(1:nrow(TrainUse), n.train)
  SummerTrainData<-TrainUse[-Usedindx,]

  TrainAvail<-SummerAll[SummerAll$Kill == 0,]
  Availindx<-sample(1:nrow(TrainAvail), 5*nrow(SummerTrainData))
  SummerAvailData<-TrainAvail[Availindx,]
  SummerTrainData<-rbind(SummerTrainData, SummerAvailData)
  SummerTrainData<-SummerTrainData[complete.cases(SummerTrainData),]

  SummerTestData<-TrainUse[Usedindx,]
  SummerAvailData<-TrainAvail[-Availindx,]
  SummerTestData<-rbind(SummerTestData, SummerAvailData)
  SummerTestData<-SummerTestData[complete.cases(SummerTestData),]


  ############ Train models #############################

  ### Parameters to Tune ####
  if(method == "repeatedcv"){
  ctrl<-caret::trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 5,
                            verboseIter = FALSE,
                            search = "random")
  }
  if(method == "bootstrap"){
  ctrl<-caret::trainControl(method = "boot", number = 50)
  }
  SummerTrainData$Kill<-as.factor(SummerTrainData$Kill)

  fincpu<-ncpu
  cluster<-snow::makeCluster(fincpu)
  doParallel::registerDoParallel(cluster)

  print(summer.tune<-train(x = SummerTrainData[, pred.names],
        y = SummerTrainData[, 'Kill'],
        method = "rf",
        metric = "Accuracy",
        trControl = ctrl))

  snow::stopCluster(cluster)
  foreach::registerDoSEQ()


  ######### Make predictions on test data ###############
  TestPred<-predict(summer.tune, SummerTestData) ## Okay, model sucks ass
  SummerTestData$Kill<-as.factor(SummerTestData$Kill)
  caret::confusionMatrix(TestPred, SummerTestData$Kill)

  ########## Build model on all data based on tuned paramaters ####
  SummerAll<-SummerAll[complete.cases(SummerAll),]
  factor_Used<-as.factor(SummerAll$Kill)
  #summer<-randomForest(SummerAll[, pred.names], factor_Used, classwt = c(10,3), mtry = 12, cutoff = c(0.8, 1-0.8))
  #summer

  fsummer<-randomForest(SummerAll[, pred.names], factor_Used, sampsize = c(317,56), mtry = 2, cutoff = c(0.75, 1-0.75))
  fsummer

  summermap<-raster::predict(rasstack, fsummer, progress= "text", type ="prob", index = 2)
  writeRaster(summermap, 'C:/Users/khuggler/Box Sync/DEER/GradStudentWork/Huggler/Chapter1/Analyses/Data/PredationRiskLayer/PredRiskSummer.img', format = "HFA",
              overwrite = TRUE)
  varImpPlot(fsummer)


  ################# Combine Winter and Summer and see if predictions are different ##################

  if(combine.data == TRUE){
    pr$NewKill<-ifelse(pr$Kill == 1 & pr$Season == "Winter", 2, ifelse(pr$Kill == 1 & pr$Season == "Summer", 3, pr$Kill))
    pr<-pr[pr$NewKill == 2 | pr$NewKill == 3,]

    p=withold
    n.train = as.integer(nrow(pr)*p)
    indx<-sample(1:nrow(pr), n.train)
    comb.data.train<-pr[-indx,]
    comb.data.train<-comb.data.train[complete.cases(comb.data.train),]

    comb.data.test<-pr[indx,]
    comb.data.test<-comb.data.test[complete.cases(comb.data.test),]

    if(method == "repeatedcv"){
      ctrl<-caret::trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 5,
                                verboseIter = FALSE,
                                search = "random")
    }
    if(method == "bootstrap"){
      ctrl<-caret::trainControl(method = "boot", number = 50)
    }
    comb.data.train$NewKill<-as.factor(comb.data.train$NewKill)

    fincpu<-ncpu
    cluster<-snow::makeCluster(fincpu)
    doParallel::registerDoParallel(cluster)

    print(all.tune<-train(x = comb.data.train[, pred.names],
                             y = comb.data.train[, 'NewKill'],
                             method = "rf",
                             metric = "Accuracy",
                             trControl = ctrl))

    snow::stopCluster(cluster)
    foreach::registerDoSEQ()

    ######### Make predictions on test data ###############
    TestPred<-predict(all.tune, comb.data.test) ## Okay, model sucks ass
    comb.data.test$NewKill<-as.factor(comb.data.test$NewKill)
    caret::confusionMatrix(TestPred, comb.data.test$NewKill)

    ########## Build model on all data based on tuned paramaters ####
    pr<-pr[complete.cases(pr),]
    factor_Used<-as.factor(pr$NewKill)
    #summer<-randomForest(SummerAll[, pred.names], factor_Used, classwt = c(10,3), mtry = 12, cutoff = c(0.8, 1-0.8))
    #summer

    all<-randomForest(pr[, pred.names], factor_Used, sampsize = c(52,56), mtry = 2, cutoff = c(0.5, 1-0.5))
    all

    ##########################################################################################################
    ## Model can't seem to differentiate between the two seasons ##
    ##########################################################################################################
    pr<-read.csv(pred.data,stringsAsFactors = F) ### Extracted data for Used and aVailable points
    pr$NLCD<-as.factor(pr$NLCD)

    p=withold
    TrainUse<-pr[pr$Kill == 1,]
    n.train = as.integer(nrow(TrainUse)*p)
    Usedindx<-sample(1:nrow(TrainUse), n.train)
    TrainData<-TrainUse[-Usedindx,]
    TrainData<-TrainData[complete.cases(TrainData),]

    TrainAvail<-pr[pr$Kill == 0,]
    Availindx<-sample(1:nrow(TrainAvail), 5*nrow(TrainData))
    AvailData<-TrainAvail[Availindx,]
    TrainData<-rbind(TrainData, AvailData)
    TrainData<-TrainData[complete.cases(TrainData),]

    TestData<-TrainUse[Usedindx,]
    Availindx<-sample(1:nrow(TrainAvail), 5*nrow(TestData))
    AvailData<-TrainAvail[Availindx,]
    TestData<-rbind(TestData, AvailData)
    TestData<-TestData[complete.cases(TestData),]


    if(method == "repeatedcv"){
      ctrl<-caret::trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 5,
                                verboseIter = FALSE,
                                search = "random")
    }
    if(method == "bootstrap"){
      ctrl<-caret::trainControl(method = "boot", number = 50)
    }
    TrainData$Kill<-as.factor(TrainData$Kill)

    fincpu<-ncpu
    cluster<-snow::makeCluster(fincpu)
    doParallel::registerDoParallel(cluster)

    print(all.tune<-train(x = TrainData[, pred.names],
                          y = TrainData[, 'Kill'],
                          method = "rf",
                          metric = "Accuracy",
                          trControl = ctrl))

    snow::stopCluster(cluster)
    foreach::registerDoSEQ()

    ######### Make predictions on test data ###############
    TestPred<-predict(all.tune, TestData) ## Okay, model sucks ass
    TestData$Kill<-as.factor(TestData$Kill)
    caret::confusionMatrix(TestPred, TestData$Kill)

    ########## Build model on all data based on tuned paramaters ####
    pr<-pr[complete.cases(pr),]
    factor_Used<-as.factor(pr$Kill)
    #summer<-randomForest(SummerAll[, pred.names], factor_Used, classwt = c(10,3), mtry = 12, cutoff = c(0.8, 1-0.8))
    #summer

    all<-randomForest(pr[, pred.names], factor_Used, sampsize = c(671,108), mtry = 7, cutoff = c(0.75, 1-0.75))
    all



    all.map<-raster::predict(rasstack, all, progress= "text", type ="prob", index = 2)
    writeRaster(all.map, 'C:/Users/khuggler/Box Sync/DEER/GradStudentWork/Huggler/Chapter1/Analyses/Data/PredationRiskLayer/PredRiskAll.img', format = "HFA",
                overwrite = TRUE)
    varImpPlot(all)

  }
}


