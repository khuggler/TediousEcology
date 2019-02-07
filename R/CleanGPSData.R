#' @title Merge Animal IDs and Sex, and Species to GPSData
#' @description Merge raw GPS Data with Animal IDs, Sex, and Species
#' @param gps gps data in the form of a data.frame or SpatialPointsDataFrame
#' @param capdat data.frame with all the capture information
#' @param dateformat format of TelemDate in gps data
#' @return Returns a data.frame with all gps data, AnimalID, Sex, and Species
#' @keywords capture, animal ID, gps, append
#' @export
#' @examples
#' \donttest{CleanData<-CleanGPSData(gps = yourgpsdata, capdat = yourcapturedata, dateformat = %Y-%m-%d)}

CleanGPSData<-function(gps, capdat, dateformat){

gps$TelemDate<-as.POSIXct(gps$TelemDate, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
gps$Date<-as.Date(gps$TelemDate, format = dateformat, tz = "MST")

x<-CollarHistory(capdat = capdat, dateformat = '%m/%d/%Y')
x$Ser1Start<-as.Date(x$Ser1Start,dateformat)
x$Ser1End<-as.Date(x$Ser1End,dateformat)
x$Ser2Start<-as.Date(x$Ser2Start,dateformat)
x$Ser2End<-as.Date(x$Ser2End,dateformat)
x$Ser3Start<-as.Date(x$Ser3Start,dateformat)
x$Ser3End<-as.Date(x$Ser3End,dateformat)
x$Ser4Start<-as.Date(x$Ser4Start,dateformat)
x$Ser4End<-as.Date(x$Ser4End,dateformat)

cap<-read.csv(capdat, stringsAsFactors = F)

for(i in 1:nrow(x)){
  xx<-x[i,]

  for(l in 1:4){
    if(l == 1){
      xxx<-xx[,1:4]
    }
    if(l == 2){
      xxx<-xx[,c(1,5,6,7)]
    }
    if(l == 3){
      xxx<-xx[,c(1,8:10)]
    }
    if(l == 4){
      xxx<-xx[,c(1,11:13)]
    }

    if(is.na(xxx[1,2])){next}
    ss<-gps[gps$CollarSerialNumber==xxx[1,2],]
    ss<-ss[(ss$Date>=xxx[,3])&ss$Date<=(xxx[,4]),]
    ss<-ss[complete.cases(ss$Latitude),]

    c<-cap[cap$UAID == xxx[1,1],]

    if(nrow(ss)==0){next}
    if(nrow(ss)>0){
      ss$AID<-xxx[1,1]
      ss$Sex<- c$Sex[1]
      ss$Spp<-c$Species[1]
    }

    if(l == 1){
      ald<-ss
    }
    if(l>1){
      ald<-rbind(ald,ss)
    }

  }
  if(i == 1){
    outsp<-ald
  }
  if(i>1){
    outsp<-rbind(outsp,ald)
  }
}

outsp<-data.frame(outsp)
outsp2<-outsp[!duplicated(outsp[,1:4]),]
outsp2<-outsp2[complete.cases(outsp2$CollarSerialNumber),]

return(outsp2)

}

