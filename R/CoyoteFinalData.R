#' @title Append AID to GPS data and subset to desired time period
#' @description Merge raw GPS Data with Animal IDs, and Sex
#' @param username username of account where gps data stored
#' @param password password of account where gps data stored
#' @param dirdown temporary directory for data to be downloaded
#' @param cType "ATS/IRID". Won't change.
#' @param yotedat path to coyote capture database
#' @param startdates vector of start dates to subset data
#' @param enddates vector of end dates to subset data
#' @return Returns a data.frame with all gps data, AnimalID, Sex,
#' @keywords coyote, animal id, movement rate
#' @export
#' @examples
#' \donttest{YoteData<-YoteGPSData(username = yourusername, password = yourpassword, dirdown = tempdir, cType = "ATS/IRID", yotedat = path, startdates = '2017-05-01', enddates = '2017-09-01')}


YoteGPSData<-function(username, password,dirdown, cType = "ATS/IRID", yotedat, startdates, enddates){
  gps<-Part::ColDownload(username = username, password = password, dirdown = dirdown, cType = cType)
  gps<-gps[[1]]
  gps<-data.frame(gps)
  yote<-read.csv(yotedat, stringsAsFactors = F)

  gps$TelemDate<-as.POSIXct(gps$TelemDate, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  gps$Date<-as.Date(gps$TelemDate, format = dateformat, tz = "MST")


  coykeep<-c('39287', '39242', '39235', '39257','39218', '39252', '39255', '39240', '39254', '39245','39248','39247','39253','39237','39239',
             '39244', '39288', '39241', '39213', '39246', '39243', '39251', '39291', '39235','39250', '39238', '39236', '39249', '39289', '39256')
  data<-subset(gps, CollarSerialNumber %in% coykeep)
  yote<-subset(yote, !Serial=="39290")

  yote$Date<-as.Date(yote$Date, format="%m/%d/%Y")
  yote$MortDate<-as.character(yote$MortDate)
  yote$MortDate<-as.Date(yote$MortDate, format = "%m/%d/%Y")
  yote$Mortality<-as.character(yote$Mortality)
  yote$MortDate<-ifelse(yote$Mortality=="1", as.character(yote$MortDate), as.character(Sys.Date()))

  uni<-unique(yote$Serial)
  f<-data.frame()
  for(i in 1:length(uni)){
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

  f<-f[f$Date >= startdates[1] & f$Date <= enddates[1] | f$Date >= startdates[2] & f$Date <= enddates[2],]
  newdat<-f

  #### Calculate Movement Rates #####
    library(adehabitatLT)
    library(data.table)
    library(sp)
    ## Tranform to UTMs ##

    coordinates(newdat) <- c("Longitude", "Latitude")
    proj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    proj4string(newdat)<-proj
    Yote<-sp::spTransform(newdat, '+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
    dimnames(Yote@coords)[[2]]<- c('Easting', "Northing")

    Yote<-data.frame(Yote)
    YoteMR<-as.ltraj(xy = Yote[, c("Easting", "Northing")], date = Yote$TelemDate, id = Yote$AID)

    YoteMR<-rbindlist(YoteMR, idcol = "id")
    ### Bind back to all coyote data ###

    Yote<-Yote[order(Yote$AID),]
    YoteMR<-YoteMR[order(YoteMR$id),]
    Yote<-cbind(Yote, YoteMR[,c(5:11)])
    Yote$dist<-Yote$dist/1000 ### transforms to km

    s<-data.frame()
    uni<-unique(Yote$AID)
    for(i in 1:length(uni)){
      sub<-Yote[Yote$AID == uni[i],]

      for(k in 1:nrow(sub)){
        sub$TimeDiff[k]<-difftime(sub$TelemDate[k+1], sub$TelemDate[k], units = "hours")
        sub$HrMR[k]<-sub$dist[k]/sub$TimeDiff[k]

        print(k)

      }
      s<-rbind(sub, s)

    }
    s$Hour<-strftime(s$TelemDate, format = "%H")
    agg<-aggregate(s$HrMR, by=list(s$Hour), FUN = mean, na.rm=T)
    plot(agg$Group.1, agg$x, type = "l", ylim = c(0.3, 1.0))

    s$act.cat<-ifelse(s$Hour >= 0 & s$Hour <= 1 | s$Hour >= 4 & s$Hour <= 7 | s$Hour >= 20 & s$Hour <= 7, "High", "Low")

    return(s)
    }







