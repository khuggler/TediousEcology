#' @title Boostrap random samples of data for RF models
#' @description Build random forest models on bootstrapped samples of data
#' @param data.frame of killsite data
#' @param idcol name of column to loop through
#' @param prop proportion of data (unique Ids) to be taken for training data
#' @param n.boot number of bootstrapped samples to take
#' @param mtry desired mtry to test in RF models
#' @param cutoff parameter of cutoff to be used in RF models
#' @param samplesize vector of sample sizes for data ex. c(50, 10)
#' @param pred.names names of predictor variables to be included in the model
#' @param cat.column column name of the category to be predicted
#' @return Returns a data.frame object with predictions of kill sites and available points
#' @keywords mountain lion, prediction, kill site, random forest
#' @export
boot.fun<-function(data, idcol, prop, n.boot, mtry, cutoff,samplesize, pred.names, cat.column){
  x<-data.frame()
  uni<-data.frame(unique(data[,idcol]))
  names(uni)<-'UniqueID'
  ss<-floor(nrow(uni)*prop)

  for(k in 1:n.boot){
  unix<-sample(uni$UniqueID, ss, replace =TRUE)
  x<-rbind(unix,x)
  }

  x<-t(x)

  d<-data.frame()
  for(l in 1:ncol(x)){
    sub<-data.frame(Uni = x[,l], k = l)

    d<-rbind(sub, d)
  }

  uni<-unique(d$k)
  fin<-data.frame()

  for(i in 1:length(uni)){
  ##### Create Train and Test Sets #####
  subsub<-d[d$k == uni[i],]
  unix<-unique(subsub$Uni)
  trainset<- data[data$ID %in% unix,]
  testset<-data[!(data$ID %in% unix),]

  trainset<-trainset[complete.cases(trainset),]
  testset<-testset[complete.cases(testset),]
  #### build RF model on data ####
  rf<-randomForest(trainset[, pred.names], as.factor(trainset[,cat.column]), sampsize = samplesize, mtry = mtry, cutoff = c(cutoff, 1-cutoff))
  varImpPlot(rf)

  #TestPred<-predict(rf, testset)
  #testset[,cat.column]<-as.factor(testset[,cat.column])
  #caret::confusionMatrix(TestPred, testset[,cat.column])

  testset$NewPred<-predict(rf, testset)
  testset$KillPred<-predict(rf, testset, type = "prob")[,2]
  testset$AvailPred<-predict(rf, testset, type = "prob")[,1]


  fin<-rbind(testset, fin)

  }
  conf<-caret::confusionMatrix(fin$NewPred, as.factor(fin[,cat.column]))
  return(list(fin, conf))
}


