#' @title Conversion of adult capture data into the correct format for modeling seasonal (winter and summer) survival
#
#' @description Convert capture and cause-specific mortality data of adults into the proper format for conducting survival analysis.This function will convert raw data
#' that contains start dates and mortality dates of individual animals and transform it into the proper format required by package survival
#' to model seasonal survival.
#' @param data data.frame that contains a column of unique animal identifier, start date, mortality date, and cause of mortality
#' @param uni vector of unique animal identifiers
#' @param UAIDcol name of column where unique animals ids are stored
#' @param mortcol name of column that contains the date of mortality
#' @param yearstart year desired to begin modeling survival (e.g. beginning year of study)
#' @param yearend year desired to end modeling survival
#' @param seasons c('winter', 'summer')
#' @param winterstart month in numeric format that is desired to start winter (e.g. 11)
#' @param winterend month in numeric format that is desired for winter to end (e.g. 05)
#' @param cause vector of causes that require censoring (e.g. collar_failure, capturemort, etc.)
#' @param format format of startcol and mortcols
#' @return Returns a data.frame with animal ID, Year of survival, start date of modeling, end date of modeling, time alive (months), and status of animal (alive/censored = 0, dead = 1)
#' @keywords adult, seasonal, winter, summer, survival, kaplan-meier, analysis
#' @export
#' @examples
#' \donttest{AdultSeasonal<-AdultSeasonalSurv(data = data, startcol = 'CaptureDate',uni = uni, UAIDcol = "UAID", mortcol = 'MortalityDate', yearstart = 2015, yearend = 2019, seasons = c('winter', 'summer'), winterstart = 11, winterend = 05, cause = c("CollarFailure", 'CaptureMort'))}

AdultSeasonalSurv<-function(data, uni, UAIDcol, startcol, mortcol, yearstart, yearend, seasons = c('winter', 'summer'), winterstart, winterend, cause, format){
  data[,mortcol]<-as.Date(data[,mortcol], format = format)
  Year<-yearstart:yearend
  season<-seasons
  c<-expand.grid(Year,seasons,KEEP.OUT.ATTRS = F)
  c$Month<-ifelse(c$Var2=='winter',winterstart,winterend)

  c$StartDate<-as.Date(paste(c$Month,01,c$Var1,sep='/'),'%m/%d/%Y')
  c<-c[order(c$StartDate),]

  c$EndDate<-NA
  c$EndDate[1:(nrow(c)-1)]<-as.character(c$StartDate[2:nrow(c)])
  names(c)<-c('Year','Season','Month','StartDate','EndDate')

  d<-data.frame()
  z<-data.frame()

  for(k in 1:length(uni)){
    sub<-data[data[UAID] == uni[k],]

    if(is.na(sub[,mortcol])){
      date <-Sys.Date()
    }
    if(!is.na(sub[,mortcol])){
    date<-as.Date(max(sub[,mortcol], na.rm=T), format = format, origin = sub[,mortcol])
    }
    start<-as.Date(min(sub[,startcol], na.rm=T), format = format, origin = sub[,startcol])
    x<-nrow(c)

    for(l in 1:x){
      xxx<-c[l,]
      #xxx$CapInd<-ifelse(start >= xxx[,4] & start < xxx[,5] | date >= xxx[,4] & date <= xxx[,5], 1, 0)
      xxx$MortInd<-ifelse(date >= xxx[,4] & date <= xxx[,5],1, 0)
      SurvTime<-ifelse(xxx$MortInd == 1, as.character(difftime(date, xxx[,4], units = "days")/30.6), 6)
      SurvTime<-as.numeric(SurvTime)
      Stat<-ifelse(m == 6 | date == Sys.Date(), 0, 1)
      #Alive<-ifelse(date == Sys.Date(), 1, 0)
      All<-data.frame(AID = sub[,UAID][1], Year = strftime(xxx[,4], format = "%Y"),
                      StartDate = xxx[,4], EndDate = xxx[,5], Time= SurvTime,
                      Status = ifelse(Stat == 0 & sub$X %in% cause, 0, Stat))

      d<-rbind(d, All)
      d<-d[d$StartDate >= start,]
      d<-d[d$StartDate <= date,]
      #d<-d[d$EndDate <= start,]
      #d<-d[d$Ind == 0,]
    }
    z<-rbind(d, z)
    z<-z[!duplicated(z[,c(1:4)]),]
  }

  return(z)
}

