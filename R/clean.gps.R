#' @title Wrapper function with CombDat and CollarHistory that Merge Animal IDs and Sex, and Species to GPSData
#' @description Merge raw GPS Data with Animal IDs, Sex, and Species
#' @param vecdata Logical. TRUE/FALSE. Whether vecdata is needed to be downloaded
#' @param vecpath path where vec data is located
#' @param usernames character vector of usernames to download
#' @param passwords character vector of passwords in same order as usernames
#' @param tempdir temporary directory for data to download (can be desktop)
#' @param ST Logical. TRUE/FALSE. Whether you need SirTrack data
#' @param STUser SirTrack Username
#' @param STPass SirTrack Password
#' @param cType "ATS/IRID" or "ATS/GSTAR"
#' @param capdat data.frame with all the capture information
#' @param dateformat format of TelemDate in gps data
#' @return Returns a data.frame with all gps data, AnimalID, Sex, and Species
#' @keywords capture, animal ID, gps, append
#' @export
#' @examples
#' \donttest{CleanData<-CleanGPSData(gps = yourgpsdata, capdat = yourcapturedata, dateformat = %Y-%m-%d)}

clean.gps<-function(vecdata, vecpath, usernames, passwords, tempdir, ST, STUser, STPass,cType, capdat, dateformat){

  if(vecdata == TRUE){
    gps<-Part::CombDat(vecpath= vecpath,
                       ATSUsers = usernames,
                       ATSPass = passwords,
                       tempdir = tempdir,
                       ST=ST,
                       STUser= STUser,
                       STPass= STPass)
  }
  if(vecdata == FALSE){
    gps<-Part::ColDownload(username = usernames,
                           password = passwords,
                           dirdown = tempdir,
                           cType = cType


    )
    gps<-gps[[1]]
  }

  gps$TelemDate<-as.POSIXct(gps$TelemDate, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  gps$Date<-as.Date(gps$TelemDate, format = dateformat, tz = "MST")

  x<-TediousEcology::colhist(capdat = capdat)
  x$Ser1Start<-as.Date(x$Ser1Start, "%Y-%m-%d")
  x$Ser1End<-as.Date(x$Ser1End,"%Y-%m-%d")
  x$Ser2Start<-as.Date(x$Ser2Start,"%Y-%m-%d")
  x$Ser2End<-as.Date(x$Ser2End,"%Y-%m-%d")
  x$Ser3Start<-as.Date(x$Ser3Start,"%Y-%m-%d")
  x$Ser3End<-as.Date(x$Ser3End,"%Y-%m-%d")
  x$Ser4Start<-as.Date(x$Ser4Start,"%Y-%m-%d")
  x$Ser4End<-as.Date(x$Ser4End,"%Y-%m-%d")
  x$Ser5Start<-as.Date(x$Ser5Start, "%Y-%m-%d")
  x$Ser5End<-as.Date(x$Ser5End, "%Y-%m-%d")

  x<-x[complete.cases(x$AID),]
  cap<-read.csv(capdat, stringsAsFactors = F)

  for(i in 1:nrow(x)){
    xx<-x[i,]
    print(i)
    for(l in 1:5){
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
      if(l == 5){
        xxx<-xx[,c(1,14:16)]
      }

      if(is.na(xxx[1,2])){next}
      ss<-gps[gps$CollarSerialNumber==xxx[1,2],]
      ss<-ss[(ss$Date> xxx[,3])&ss$Date< (xxx[,4]),]
      ss<-ss[complete.cases(ss$Latitude),]

      c<-cap[cap$UAID == xxx[1,1],]

      #ss<-as.data.frame(ss)
      if(nrow(ss)==0){next}
      if(nrow(ss)>0){
        ss$AID<-xxx[1,1]
        ss$Sex<- c$Sex[1]
        ss$Spp<-c$Species[1]
        ss$FawnAID<-c$FawnAID[1]
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
