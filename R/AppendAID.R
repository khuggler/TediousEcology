#' @title Append Animal IDs created from CollarHistory function to original capture database
#' @description Append unique animals IDs created from CollarHistory function to capture database.
#' @param capdat path to capture database
#' @param capturedate name of column with capture date
#' @param mortalitydate name of column with mortality date
#' @param dateformat format of dates in database (character)
#' @param colhist data.frame created from CollarHistory function
#' @param fileout path to file and name of file where appended database shoudl be stored
#' @return Returns a data.frame with original capture data.frame and a column appended with UAID
#' @keywords adult, animal ID, capture, database
#' @export
#' @examples
#' \donttest{DBWithAID<-AppendAID(capdat = 'C:/Users/khuggler/Box Sync/DEER/Data/CleanDatabase/DB_WithAID_02082019.csv', capturedate = "CaptureDate", mortalitydate = "MortalityDate", 'dateformat = "%m/%d/%Y', colhist = ColHist, fileout = 'C:/Users/khuggler/Box Sync/DEER/Data/CleanDatabase/DB_WithAID_02082019.csv')}

AppendAID<-function(capdat, capturedate, mortalitydate, dateformat, colhist, fileout){
dat<-read.csv(capdat,stringsAsFactors = F)
dat$CaptureDate<-as.Date(dat[,capturedate],dateformat)
dat$MortalityDate<-as.Date(dat[,mortalitydate],dateformat)

uaid<-colhist
uaid$Ser1Start<-as.Date(uaid$Ser1Start,'%Y-%m-%d')
uaid$Ser1End<-as.Date(uaid$Ser1End,'%Y-%m-%d')
uaid$Ser2Start<-as.Date(uaid$Ser2Start,'%Y-%m-%d')
uaid$Ser2End<-as.Date(uaid$Ser2End,'%Y-%m-%d')
uaid$Ser3Start<-as.Date(uaid$Ser3Start,'%Y-%m-%d')
uaid$Ser3End<-as.Date(uaid$Ser3End,'%Y-%m-%d')
uaid$Ser4Start<-as.Date(uaid$Ser4Start,'%Y-%m-%d')
uaid$Ser4End<-as.Date(uaid$Ser4End,'%Y-%m-%d')

#dat$AID<-NA

names(uaid)<-c('UAID','Serial','StartDate','EndDate',
               'Serial','StartDate','EndDate',
               'Serial','StartDate','EndDate',
               'Serial','StartDate','EndDate')
alid<-rbind(uaid[,1:4],uaid[,c(1,5,6,7)],uaid[,c(1,8:10)],uaid[,c(1,11:13)])
alid$Serial<-as.character(alid$Serial)

dat$Old.Serial.Number<-as.character(dat$Old.Serial.Number)
dat$New.Serial.Number<-as.character(dat$New.Serial.Number)
# dat$New.Serial.Number<-ifelse(is.na(dat$New.Serial.Number),dat$Old.Serial.Number,dat$New.Serial.Number)

dat$UAID<-NA



for(k in 1:nrow(dat)){
  ds<-dat[k,]



  for(i in 1:nrow(uaid)){
    ss<-uaid[i,]
    if(!(is.na(dat[k,'UAID']))){break}
    dat[k,'UAID']<-ifelse(ds$New.Serial.Number==ss[,2]&as.numeric(difftime(ds$CaptureDate,ss[,3],units='days'))==0&is.na(ds$Old.Serial.Number),ss[,1],NA)
    if(!(is.na(dat[k,'UAID']))){break}
    dat[k,'UAID']<-ifelse(ds$New.Serial.Number==ss[,5]&as.numeric(difftime(ds$CaptureDate,ss[,6],units='days'))==0&((ds$Old.Serial.Number==ss[,2])|(ds$CaptureDate>ss[,6]&ds$CaptureDate<ss[,7])|(ds$Mortality.==1&ds$CaptureDate>=ss[,6]&ds$CaptureDate<=ss[,7]&ds$MortalityDate==ss[,7])),ss[,1],NA)
    if(!(is.na(dat[k,'UAID']))){break}
    dat[k,'UAID']<-ifelse(ds$New.Serial.Number==ss[,8]&as.numeric(difftime(ds$CaptureDate,ss[,9],units='days'))==0&((ds$Old.Serial.Number==ss[,5])|(ds$CaptureDate>ss[,9]&ds$CaptureDate<ss[,10])|(ds$Mortality.==1&ds$CaptureDate>=ss[,9]&ds$CaptureDate<=ss[,10]&ds$MortalityDate==ss[,10])),ss[,1],NA)
    if(!(is.na(dat[k,'UAID']))){break}
    dat[k,'UAID']<-ifelse(ds$New.Serial.Number==ss[,11]&as.numeric(difftime(ds$CaptureDate,ss[,12],units='days'))==0&((ds$Old.Serial.Number==ss[,8])|(ds$CaptureDate>ss[,12]&ds$CaptureDate<ss[,13])|(ds$Mortality.==1&ds$CaptureDate>=ss[,12]&ds$CaptureDate<=ss[,13]&ds$MortalityDate==ss[,13])),ss[,1],NA)
    if(!(is.na(dat[k,'UAID']))){break}
    dat[k,'UAID']<-ifelse(ds$Old.Serial.Number==ss[,2]&ds$CaptureDate>=ss[,3]&ds$CaptureDate<=ss[,4],ss[,1],NA)
    if(!(is.na(dat[k,'UAID']))){break}
    dat[k,'UAID']<-ifelse(ds$Old.Serial.Number==ss[,5]&ds$CaptureDate>=ss[,6]&ds$CaptureDate<=ss[,7]&((ds$Old.Serial.Number==ss[,2])|(ds$CaptureDate>ss[,6]&ds$CaptureDate<ss[,7])|(ds$Mortality.==1&ds$CaptureDate>=ss[,6]&ds$CaptureDate<=ss[,7]&ds$MortalityDate==ss[,7])),ss[,1],NA)
    if(!(is.na(dat[k,'UAID']))){break}
    dat[k,'UAID']<-ifelse(ds$Old.Serial.Number==ss[,8]&ds$CaptureDate>=ss[,9]&ds$CaptureDate<=ss[,10]&((ds$Old.Serial.Number==ss[,5])|(ds$CaptureDate>ss[,9]&ds$CaptureDate<ss[,10])|(ds$Mortality.==1&ds$CaptureDate>=ss[,9]&ds$CaptureDate<=ss[,10]&ds$MortalityDate==ss[,10])),ss[,1],NA)
    if(!(is.na(dat[k,'UAID']))){break}
    dat[k,'UAID']<-ifelse(ds$Old.Serial.Number==ss[,11]&ds$CaptureDate>=ss[,12]&ds$CaptureDate<=ss[,13]&((ds$Old.Serial.Number==ss[,8])|(ds$CaptureDate>ss[,12]&ds$CaptureDate<ss[,13])|(ds$Mortality.==1&ds$CaptureDate>=ss[,12]&ds$CaptureDate<=ss[,13]&ds$MortalityDate==ss[,13])),ss[,1],NA)



  }
  print(k)
}


dat$N<-1:nrow(dat)


maxN<-max(uaid$UAID,na.rm=T)+1

for(i in 1:nrow(dat)){
  if(is.na(dat$UAID[i])){
    dat$UAID[i]<-maxN
    maxN<-maxN+1
  }

}
write.csv(dat, fileout, row.names = F)

return(dat)
}

