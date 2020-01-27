#' @title Append AID to GPS data and subset to desired time period
#' @description Merge raw GPS Data with Animal IDs, and Sex
#' @param username username of account where gps data stored
#' @param password password of account where gps data stored
#' @param dirdown temporary directory for data to be downloaded
#' @param cType "ATS/IRID". Won't change.
#' @param yotedat path to coyote capture database
#' @return Returns a data.frame with all gps data, AnimalID, Sex,
#' @keywords coyote, animal id, movement rate
#' @export
#' @examples
#' \donttest{yote.gps<-yote.gps(username = yourusername, password = yourpassword, dirdown = tempdir, cType = "ATS/IRID", yotedat = path, startdates = '2017-05-01', enddates = '2017-09-01')}


yote.gps<-function(username, password,dirdown, cType = "ATS/IRID", yotedat){
  
  #read in gps data#
  gps<-Part::ColDownload(username = username, password = password, dirdown = dirdown, cType = cType)
  gps<-gps[[1]]
  gps<-data.frame(gps)
  
  #get capture history#
  yote<-read.csv(yotedat, stringsAsFactors = F)

  gps$TelemDate<-as.POSIXct(gps$TelemDate, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  gps$Date<-as.Date(gps$TelemDate, format = "%Y-%m-%d", tz = "MST")
  
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
