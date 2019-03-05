#' @title bind mom aid to neonate capture data
#' @description bind mom aid to neonate capture database
#' @param neopath path to neonate database
#' @param mompath path to mom capture database
#' @param pathout path to write new neonate databse
#' @return Returns a data.frame with original neonate capture database and mom's AID
#' @keywords aid, neonate, database, append
#' @export

append.mom.to.neo<-function(neopath, mompath, pathout){

neo<-read.csv(nepath, stringsAsFactors = F) ## read in neonates
mom<-read.csv(mompath, stringsAsFactors = F) ## read in moms

neo$StartDate<-as.Date(neo$StartDate, tryFormats = c("%m/%d/%Y", '%Y-%m-%d'))
mom$CaptureDate<-as.Date(mom$CaptureDate, tryFormats = c("%m/%d/%Y", '%Y-%m-%d'))
mom$MortalityDate<-as.Date(mom$MortalityDate, tryFormats = c("%m/%d/%Y", '%Y-%m-%d'))

neo$Year<-strftime(neo$StartDate, format = "%Y") ## build unique identifier
mom$Year<-strftime(mom$CaptureDate, format = "%Y")
mom$CapMonth<-strftime(mom$CaptureDate, format = '%m')
mom$MoYr<-paste(mom$CapMonth, mom$Year, sep = "_")

mom<-mom[mom$MoYr == "04_2016" | mom$MoYr == "04_2017" | mom$MoYr == "04_2018",] ## just easiest to loop through moms this way
mom$Ser<-ifelse(is.na(mom$Old.Serial.Number), mom$New.Serial.Number, ifelse(!is.na(mom$Old.Serial.Number) & !is.na(mom$New.Serial.Number), mom$New.Serial.Number, mom$Old.Serial.Number))
################################################# Fix AIDs for DB ################################
neo$MomYr<-paste(neo$Year, neo$MomSerial, sep = "_") ## unique id to loop through
mom$MomYr<-paste(mom$Year, mom$Ser, sep = "_") ## id to match

uni<-unique(neo$MomYr)

d<-data.frame()
for(i in 1:length(uni)){
  sub<-mom[mom$MomYr == uni[i],]
  if(nrow(sub)== 0){next}
  if(nrow(sub) > 1){
    sub<-sub[!is.na(sub$MortalityDate),]
  }
  neosub<-neo[neo$MomYr == uni[i],]
  neosub$MomAID<-sub$UAID
  d<-rbind(d, neosub)
}

Neo<-d
Neo<-Neo[,c(1:40, 41, 42)]
Neo$FID<-paste(d$Year, d$MomAID, sep = "_")
Neo$FID<-paste(Neo$FID, d$FawnID, sep = "_")

write.csv(Neo, pathout, row.names = F)
return(Neo)
}
