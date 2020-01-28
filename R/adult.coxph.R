#' @title Create data with age and IFBFat to model survival
#' @description Create data with age and IFBFat to model survival
#' @param data database with age column in it
#' @param uni vector of unique ids in which survival should be calculated
#' @param UAIDcol column where aids live
#' @param startcol name of column where capture date lives
#' @param mortcol name of column where mortality date lives
#' @param yearstart path to study area polygon (where you want to define availability)
#' @param yearend path to where predicted RF map should be written
#' @param seasons duh
#' @param winterstart duh
#' @param winterend duh
#' @param cause duh
#' @param plot duh
#' @param title duh
#' @param grossweightcol name of column where gross weight is stored
#' @param maxthickcol name of column where max thickness is stored
#' @param bcscol name of column where bcs is stored
#' @param seasoncol name of column where season (Fall/Spring) is located
#' @return Returns a with survival data and appended ages and IFBFat
#' @keywords IFBFat, age, adult, survival, seasonal
#' @export


adult.coxph<-function(data, uni, UAIDcol,startcol,mortcol, yearstart, yearend, seasons, winterstart, winterend, cause, plot, title, grossweightcol, maxthickcol, bcscol, seasoncol){

  survdat<-TediousEcology::AdultSeasonalSurv(data = data, uni = uni, UAIDcol = UAIDcol, startcol = startcol, mortcol = mortcol, yearstart = yearstart,
                                             yearend = yearend, seasons = seasons, winterstart = winterstart, winterend = winterend,
                                             cause = cause, plot = plot, title = title)

  surv.dat<-survdat[[2]]
  surv.dat$aidyr<-paste(surv.dat$AID, surv.dat$SeasonYr, sep = "_")

  data[,startcol]<-as.Date(data[,startcol], tryFormats = c('%m/%d/%Y', '%Y-%m-%d'))
  data$month<-strftime(data[,startcol], format = "%m")
  data$Season<-ifelse(data$month == "11" | data$month == "12", "winter", "summer")
  data$Year<-strftime(data[,startcol], format = '%Y')
  data$SeasonYr<-paste(data$Season, data$Year, sep = "_")
  data$aidyr<-paste(data$UAID, data$SeasonYr, sep = "_")
  ifbdat<-TediousEcology::IFBFat(x = data, weight = 'lbs', GrossweightColName = grossweightcol, maxthickName = maxthickcol, bcsName = bcscol, FetNum = 1.692, season = seasoncol)


  ### bind ifbdat to survival data ###
  uni<-unique(ifbdat$aidyr)

  full.surv<-data.frame()
  for(k in 1:length(uni)){
    survsub<-surv.dat[surv.dat$aidyr == uni[k],]
    if(nrow(survsub) == 0){next}
    ifbsub<-ifbdat[ifbdat$aidyr == uni[k],]
    ifbsub<-ifbsub[!duplicated(ifbsub$aidyr),]

    survsub$ifbf<-ifbsub$scaledIFBFat
    survsub$age<-ifbsub$new.age

    full.surv<-rbind(survsub, full.surv)
  }
return(full.surv)
}

