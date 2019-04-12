#' @title Calculate IFBFat
#' @description Calculate IFBFat from weight, max thickness, body condition score, and fetal number.
#' @param x capture database
#' @param weight 'lbs'
#' @param GrossewightColName name of column where gross weight is stored
#' @param maxthickName name of column where max thickness is stored
#' @param bcsName name of column where body condition score is stored
#' @param FetNum path to study area polygon (where you want to define availability)
#' @param season name of column where season (Fall/Spring) is stored
#' @return Returns a original dataframe with appended columns of IFBFat and Scaled IFBFat
#' @keywords IFBFat, body condition, max thicknes, weight
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
