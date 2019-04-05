#' @title Stack and extract all spatial data to elk, coyotes, and mountain lions (RF Encounter)
#' @description Stack rasters and extract points data for elk, coyotes, and mountain lions
#' @param x data to be used to build RF model
#' @param weight number of cpus to parallelize on
#' @param GrossewightColName percent of data (decimal format) to withold for training model (default 0.25)
#' @param maxthickName number of trees (maximum) to attempt to split on
#' @param bcsName path to raster stack
#' @param FetNum path to study area polygon (where you want to define availability)
#' @param season path to where predicted RF map should be written
#' @return Returns a list object with RFData necessary to predict probably of use in RF models (elk, coyotes, and mountain lions)
#' @keywords elk, coyote, mountain lion, random forest, extract, raster, sample
#' @export

IFBFat<-function(x,weight='lbs',
                 GrossweightColName='Gross.Weight..lb.',
                 maxthickName='MaxThickness',
                 bcsName='Body.Condition.Score',
                 FetNum=1.692,
                 season='Fall'){

  # net=2.5
  # bag=2
  # eye cover=0.2
  # litter=4.2
  # hobbles=0.5

  if(season=='Fall'){
  if(weight=='lbs'){
    x$NetWeightKG<-(x[,GrossweightColName]-4.263765)*0.453592
  }

  x<-x[complete.cases(x$NetWeightKG),]
  x$ScaledMaxFat<-(x[,maxthickName]/10)/(0.142*(x$NetWeightKG^0.63))




  livifun<-function(x){
    if(x[1]<1){
      xl<-3.869*x[2]-2.71
    }else{
      xl<-11.35*x[1]+5.63
    }
    return(xl)
  }
  x<-x[complete.cases(x$ScaledMaxFat),]

  x$LIVINDEX<-apply(x[,c('ScaledMaxFat',bcsName)],1,livifun)

  x$scaledIFBFat<- -0.16+1.01*x$LIVINDEX


  return(x)
  }else{
    if(weight=='lbs'){
      x$NetWeightKG<-(x[,GrossweightColName]-6.6)*0.453592
    }

    x<-x[complete.cases(x$NetWeightKG),]
    x$ScaledMaxFat<-(x[,maxthickName]/10)/(0.142*((x$NetWeightKG-(3.04*FetNum))^0.63))




    livifun<-function(x){
      if(x[1]<1){
        xl<-3.869*x[2]-2.71
      }else{
        xl<-11.35*x[1]+5.63
      }
      return(xl)
    }
    x<-x[complete.cases(x$ScaledMaxFat),]

    x$LIVINDEX<-apply(x[,c('ScaledMaxFat',bcsName)],1,livifun)

    x$scaledIFBFat<- -0.16+1.01*x$LIVINDEX


    return(x)
  }

}
