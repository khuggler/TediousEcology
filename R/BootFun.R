#' @title Boostrap random samples of data for RF models
#' @description Build random forest models on bootstrapped samples of data
#' @param data.frame of killsite data
#' @param sampsize proportion of data (unique Ids) to be taken for training data
#' @param n.boot number of bootstrapped samples to take
#' @param mtry desired mtry to test in RF models
#' @param cutoff parameter of cutoff to be used in RF models
#' @param pred.names names of predictor variables to be included in the model
#' @param cat.column column name of the category to be predicted
#' @return Returns a data.frame object with predictions of kill sites and available points
#' @keywords mountain lion, prediction, kill site, random forest
#' @export
boot.fun<-function(data, sampsize, n.boot, mtry, cutoff, pred.names,catcolumn){
  x<-data.frame()
  uni<-data.frame(unique(data$ID))
  ss<-floor(nrow(uni)*sampsize)

  for(k in 1:n.boot){
  unix<-sample(uni$unique.data.ID., ss, replace =TRUE)
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

  #### build RF model on data ####
  rf<-randomForest(trainset[, pred.names], as.factor(trainset[,catcolumn]), sampsize =c(50, 10), mtry = mtry, cutoff = c(cutoff, 1-cutoff))
  varImpPlot(rf)

  testset$KillPred<-predict(rf, testset, type = "prob")[,2]
  testset$AvailPred<-predict(rf, testset, type = "prob")[,1]


  fin<-rbind(testset, fin)
  }
  return(fin)
}


