#' @title Append AID to GPS data and subset to desired time period
#' @description Merge raw GPS Data with Animal IDs, and Sex
#' @param gps object identifying gps data file
#' @param yotedat path to coyote capture database
#' @return Returns a data.frame with all gps data, AnimalID, Sex,
#' @keywords coyote, animal id, movement rate
#' @export




yote.gps<-function(yotedat, gps){
  
  
  
  #get capture history#
  yote<-read.csv(yotedat, stringsAsFactors = F)
  
  gps$Date<-as.character(gps$Date)
  gps$Date<-as.Date(gps$Date, format = "%Y-%m-%d")
  
  gps$TelemDate<-paste0(gps$Date, " ", gps$Hour, ":", gps$Minute)
  gps$TelemDate<-as.POSIXct(gps$TelemDate, format = "%Y-%m-%d %H:%M", tz = "GMT")
  attr(gps$TelemDate, "tzone")<-"MST"
  
  uni<-unique(yote$Serial)
  
  #only pull gps data for animals that were ever captured#
  data<-subset(gps, CollarSerialNumber %in% uni)
  
  # fix dates #
  yote$Date<-as.Date(yote$Date, format="%m/%d/%Y")
  yote$MortDate<-as.Date(yote$MortDate, format = "%m/%d/%Y")
  yote$MortDate<-ifelse(is.na(yote$MortDate), as.character(Sys.Date()), as.character(yote$MortDate))
  yote$MortDate<-as.Date(yote$MortDate, format = '%Y-%m-%d')
  
  #loop through capture database and append AIDs#
  
  f<-data.frame()
  for(i in 1:length(uni)){
    
    if(uni[i] == "39290"){next}
    
    sub<-gps[gps$CollarSerialNumber == uni[i],]
    subsub<-yote[yote$Serial == uni[i],]
    
    if(nrow(subsub) == 1){
      sub<-sub[sub$Date >= subsub$Date & sub$Date <= subsub$MortDate,]
      sub$AID<-subsub$AID
      sub$Sex<-subsub$Sex
    }
    
    if(nrow(subsub)> 1){
      b<-nrow(subsub)
      for(k in 1:length(b)){
        sub$AID<-ifelse(sub$Date >= subsub$Date[k]+1 & sub$Date <= subsub$MortDate[k], subsub$AID[k], NA)
        sub$Sex<-ifelse(sub$Date >= subsub$Date[k]+1 & sub$Date <= subsub$MortDate[k], subsub$Sex[k], NA)
        sub$AID<-ifelse(sub$Date >= subsub$Date[k+1]+1 & sub$Date <= subsub$MortDate[k+1], subsub$AID[k+1], sub$AID)
        sub$Sex<-ifelse(sub$Date >= subsub$Date[k+1]+1 & sub$Date <= subsub$MortDate[k+1], subsub$Sex[k+1], sub$Sex)
        
      }
    }
    
    f<-rbind(sub, f)
    f<-f[complete.cases(f$AID),]
    
    
  }
  
  return(f)
}
