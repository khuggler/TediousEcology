#' @title Wrapper function with CombDat and CollarHistory that Merge Animal IDs and Sex, and Species to GPSData
#' @description Merge raw GPS Data with Animal IDs, Sex, and Species
#' @param atsfold path to folder where ats data is located
#' @param vecpath path where vec data is located
#' @param capdat path to capture database
#' @return Returns a data.frame with all gps data, AnimalID, Sex, and Species
#' @keywords capture, animal ID, gps, append
#' @export



clean.gps<-function(atsfold, vecpath, capdat){

#' =============================
#' read in ATS data 
#' =============================
#' 
files<-list.files(atsfold, full.names = T)

atsdat<-data.frame()
for(k in 1:length(files)){
  sub<-read.table(files[[k]], sep = ",", header = T)
  
  atsdat<-rbind(sub, atsdat)
}

atsdat$Date<-as.Date(atsdat$Date, format = "%m/%d/%Y")
atsdat$Hour<-sprintf("%02d", atsdat$Hour)
atsdat$Minute<-sprintf("%02d", atsdat$Minute)
atsdat$time<-paste0(atsdat$Hour, ":", atsdat$Minute, ":", "00")
atsdat$TelemDate<-paste(atsdat$Date, atsdat$time, sep = " ")
atsdat$TelemDate<-as.POSIXct(atsdat$TelemDate, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

attributes(atsdat$TelemDate)$tzone<-'MST'

#' ==========================================
#' read in vectronics data and bind together
#' =========================================
#' 

vec<-Part::getVec(vecpath)
vec<-data.frame(vec)

#' ======================================
#' merge vectronics and ats data 
#' ======================================
vec<-vec[, c(1:7)]
names(vec)[6:7]<-c('Latitude', 'Longitude')
ats<-atsdat[, c(1,16, 10, 13, 7, 8, 9)]

gps<-rbind(vec, ats)
gps$Date<-strftime(gps$TelemDate, format = "%Y-%m-%d")

#' =============================
#' create collar history table
#' =============================


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
    ss<-ss[(ss$Date> xxx[,3] + 7)&ss$Date< (xxx[,4] - 7),]
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
