#' @title Wrapper function with CombDat and CollarHistory that Merge Animal IDs and Sex, and Species to GPSData
#' @description Merge raw GPS Data with Animal IDs, Sex, and Species
#' @param atsfold path to folder where ats data is located
#' @param vecpath path to where vectronics keys are located
#' @param capdat path to capture database
#' @param spp either "cervid" or "coyote"
#' @param buffer.capture Logical. True if you would like to remove locations surrounding captures (user defined buffer in days) False if you would like to include captures in gps data 
#' @param buffer numeric number of days to buffer around captures
#' @return Returns a data.frame with all gps data, AnimalID, Sex, and Species
#' @keywords capture, animal ID, gps, append
#' @export
#' @examples

cleangps<-function(atsfold, veckeys, capdat, spp, buffer.capture, buffer){

#' =============================
#' read in ATS data
#' =============================
#'

  if(spp == "cervid"){
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

if(!'collar' %in% installed.packages()){
  devtools::install_github("Huh/collar", force = T)
}

if('collar' %in% installed.packages()){
  require(collar)
}


key_path <- collar::get_paths(veckeys)
vec<-collar::fetch_vectronics(key_path, type = "gps")
vec<-data.frame(vec)

vec$acquisitiontime<-as.POSIXct(vec$acquisitiontime, format = paste0("%Y-%m-%d", "T", "%H:%M:%S"), tz = "UTC", origin = vec$acquisitiontime)
vec$acquisitiontime<-format(vec$acquisitiontime, tz = "MST", usetz = FALSE)


#' ======================================
#' merge vectronics and ats data
#' ======================================
vec<-vec[, c(2,3,12,13,44,9,10)]
ats<-atsdat[, c(1,16, 10, 13, 7, 8, 9)]
ats$TelemDate<-as.POSIXct(ats$TelemDate, format = "%Y-%m-%d %H:%M:%S")
names(vec)<-names(ats)
vec$TelemDate<-as.POSIXct(vec$TelemDate, format = "%Y-%m-%d %H:%M:%S")

gps<-rbind(vec, ats)
gps$TelemDate<-as.POSIXct(gps$TelemDate, format = "%Y-%m-%d %H:%M:%S")
gps$Date<-strftime(gps$TelemDate, format = "%Y-%m-%d")
gps$Date<-as.Date(gps$Date, format = "%Y-%m-%d")

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
outsp<-data.frame()

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
    #ss<-ss[complete.cases(ss$CollarSerialNumber),]
    
    #if(nrow(ss) == 0){next}
    
    #if(nrow(ss) > 0){
    
      if(buffer.capture == TRUE){
      ss<-ss[(ss$Date> xxx[,3] + buffer)&ss$Date< (xxx[,4] - buffer),]
      }
      
      if(buffer.capture == FALSE){
        ss<-ss[(ss$Date>= xxx[,3])&ss$Date <= (xxx[,4]),]
      }
      
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
outsp<-outsp[complete.cases(outsp$CollarSerialNumber),]

return(outsp)
  }

  if(spp == "coyote"){

      files<-list.files(atsfold, full.names = T)
      atsdat<-read.table(files, sep = ",", header = T)

     atsdat$Date<-as.Date(atsdat$Date, format = "%m/%d/%Y")
    atsdat$Hour<-sprintf("%02d", atsdat$Hour)
    atsdat$Minute<-sprintf("%02d", atsdat$Minute)
    atsdat$time<-paste0(atsdat$Hour, ":", atsdat$Minute, ":", "00")
    atsdat$TelemDate<-paste(atsdat$Date, atsdat$time, sep = " ")
    atsdat$TelemDate<-as.POSIXct(atsdat$TelemDate, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

    attributes(atsdat$TelemDate)$tzone<-'MST'
    gps<-atsdat



    yote<-read.csv(capdat, stringsAsFactors = F)
    uni<-unique(yote$Serial)

    #only pull gps data for animals that were ever captured#
    data<-subset(gps, CollarSerialNumber %in% uni)

    # fix dates #
    yote$Date<-as.Date(yote$Date, format="%m/%d/%Y")
    yote$MortDate<-as.Date(yote$MortDate, format = "%m/%d/%Y")
    yote$MortDate<-ifelse(is.na(yote$MortDate), as.character(Sys.Date()), as.character(yote$MortDate))
    yote$MortDate<-as.Date(yote$MortDate, format = '%Y-%m-%d')

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

}
