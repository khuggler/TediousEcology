#' @title Conversion of adult capture data into the correct format for modeling seasonal (winter and summer) survival
#' @description Convert capture and cause-specific mortality data of adults into the proper format for conducting survival analysis.This function will convert raw data
#' that contains start dates and mortality dates of individual animals and transform it into the proper format required by package survival
#' to model seasonal survival.
#' @param data data.frame that contains a column of unique animal identifier, start date, mortality date, and cause of mortality
#' @param uni vector of unique animal identifiers
#' @param UAIDcol name of column where unique animals ids are stored
#' @param startcol name of column with capture date
#' @param mortcol name of column that contains the date of mortality
#' @param yearstart year desired to begin modeling survival (e.g. beginning year of study)
#' @param yearend year desired to end modeling survival
#' @param seasons c('winter', 'summer')
#' @param winterstart month in numeric format that is desired to start winter (e.g. 11)
#' @param winterend month in numeric format that is desired for winter to end (e.g. 05)
#' @param cause vector of causes that require censoring (e.g. collar_failure, capturemort, etc.)
#' @param dateformat format of startcol and mortcols
#' @param plot Logical. TRUE/FALSE. Whether a barplot of cumulative survival is desired
#' @param title desired title of plot
#' @return Returns a data.frame with animal ID, Year of survival, start date of modeling, end date of modeling, time alive (months), and status of animal (alive/censored = 0, dead = 1)
#' @keywords adult, seasonal, winter, summer, survival, kaplan-meier, analysis
#' @export
#' @examples
#' \donttest{AdultSeasonal<-AdultSeasonalSurv(data = data, startcol = 'CaptureDate',uni = uni, UAIDcol = "UAID", mortcol = 'MortalityDate', yearstart = 2015, yearend = 2019, seasons = c('winter', 'summer'), winterstart = 11, winterend = 05, cause = c("CollarFailure", 'CaptureMort'))}

AdultSeasonalSurv<-function(data, uni, UAIDcol, startcol, mortcol, yearstart, yearend, seasons = c('winter', 'summer'), winterstart, winterend, cause, dateformat, plot, title){
  
  data[,mortcol]<-as.Date(data[,mortcol], tryFormats = c('%m/%d/%Y', '%Y-%m-%d'))
  data[,startcol]<-as.Date(data[,startcol], tryFormats = c('%m/%d/%Y', '%Y-%m-%d'))
  Year<-yearstart:yearend
  season<-seasons
  c<-expand.grid(Year,seasons,KEEP.OUT.ATTRS = F)
  c$Month<-ifelse(c$Var2=='winter',winterstart,winterend)

  c$StartDate<-as.Date(paste(c$Month,01,c$Var1,sep='/'),'%m/%d/%Y')
  c<-c[order(c$StartDate),]

  c$EndDate<-NA
  c$EndDate[1:(nrow(c)-1)]<-as.character(c$StartDate[2:nrow(c)])
  names(c)<-c('Year','Season','Month','StartDate','EndDate')
  c<-c[2:nrow(c),]

  d<-data.frame()
  z<-data.frame()

  for(k in 1:length(uni)){
    sub<-data[data[,UAIDcol] == uni[k],]

    p<-nrow(sub)

    if(is.na(sub[p,mortcol])){
      date<-Sys.Date()
    }
    if(!is.na(sub[p,mortcol])){
    date<-as.Date(max(sub[,mortcol], na.rm=T), format = dateformat, origin = sub[,mortcol])
    }
    start<-as.Date(min(sub[,startcol], na.rm=T), format = dateformat, origin = sub[,startcol])
    x<-nrow(c)

    
    for(l in 1:x){
      xxx<-c[l,]
      xxx$Mort<-ifelse(date >= xxx[,4] & date <= xxx[,5], 1,0)
      xxx$Start<-ifelse(start >= xxx[,4] & start <= xxx[,5], 1, 0)
      xxx$startdiff<-ifelse(xxx$Start == 1, as.numeric(difftime( xxx[,5],start, units = "days")/30.6), 6)
      xxx$diff<-ifelse(xxx$Mort == 1, as.numeric(difftime(date, xxx[,4], units = "days")/30.6), 6)
      xxx$status<-ifelse(xxx$diff == 6 | date == Sys.Date(), 0 , 1)
      xxx$censor<-ifelse(sub$X[p] %in% cause, 1, 0)
      
      
      All<-data.frame(AID = sub[,UAIDcol][1], Year = strftime(xxx[,4], format = "%Y"),
                      StartDate = xxx[,4], EndDate = xxx[,5], Start = xxx$Start, StartTime = xxx$startdiff, Time= xxx$diff,
                      Status = ifelse(xxx$status == 0 | sub$X[p] %in% cause, 0, 1),
                      SeasonYr = paste(xxx[,2], strftime(xxx[,4], format = "%Y"), sep = "_"))
      
      
      d<-rbind(d, All)
    }
    
    d$AID<-as.character(d$AID)
    subsub<-data.frame()
    unix<-unique(d$AID)
    
    for(i in 1:length(unix)){
      
      sub2<-d[d$AID == unix[i],]
      start.row<-which(sub2$Start == 1)
      death.row<-which(sub2$Status == 1)
      death.row<-ifelse(length(death.row)==0, nrow(d), death.row)
      
      rows<-seq(start.row, death.row, 1)
      sub2<-sub2[rows, ]
      
      subsub<-rbind(sub2, subsub)
    }
      
    print(k)
    
    z<-rbind(subsub, z)
   z<-z[complete.cases(z),]
   z<-z[!duplicated(z[,1:4]),]
   
   z$Time<-ifelse(z$Start == 1 & z$StartTime != 6, z$StartTime, z$Time)
    
  }
    
  
cumsurv<-data.frame()
  if(plot == TRUE){
    surs<-survival::survfit(survival::Surv(time = z$Time, event = z$Status)~z$SeasonYr)

    summ<-summary(surs)
    cols<-lapply(c(2:6, 8:11), function(x) summ[x])
    tbl<-do.call(data.frame, cols)

    unistrat<-as.character(unique(tbl$strata))

    cumsurv<-data.frame()

    for (h in 1:length(unistrat)){
      subsurv<-tbl[tbl$strata == unistrat[h],]
      row<-nrow(subsurv)
      subsurv<-subsurv[row,]
      subsurv$strata<-as.character(subsurv$strata)
      subsurv$SeasonYear<-strsplit(subsurv$strata, split = "=")[[1]][2]
      subsurv$Season<-strsplit(subsurv$SeasonYear, split = "_")[[1]][1]
      subsurv$Year<-strsplit(subsurv$strata, split = "_")[[1]][2]

      cumsurv<-rbind(subsurv, cumsurv)
      cumsurv<-cumsurv[order(cumsurv$Year),]

    }

    #library(RColorBrewer)
    #n<-length(unique(cumsurv$Season))
    #cols<-brewer.pal(n = n, name = "Set1")
    cumsurv$Year<-as.numeric(cumsurv$Year)
    cumsurv<-cumsurv[order(cumsurv$Year, cumsurv$Season),]
    cumsurv<-cumsurv[complete.cases(cumsurv$surv),]
    barplot(cumsurv$surv, col = c("red", "blue"), ylim = c(0,1),
            names.arg = cumsurv$SeasonYear, border = NA, main = title, beside = TRUE)


  }
  return(list(cumsurv, z))
}

