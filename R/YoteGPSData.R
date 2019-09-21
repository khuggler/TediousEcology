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
  gps$Date<-as.Date(gps$TelemDate, format = "%Y-%m-%d", tz = "MST")

  uni<-unique(yote$CollarSerialNumber)
  data<-subset(gps, CollarSerialNumber %in% uni)
  #yote<-subset(yote, !Serial=="39290")
  
  
  yote$Date<-as.Date(yote$Date, format="%m/%d/%Y")
  yote$MortDate<-as.character(yote$MortDate)
  yote$MortDate<-as.Date(yote$MortDate, format = "%m/%d/%Y")
  yote$Mortality<-as.character(yote$Mortality)
  yote$MortDate<-ifelse(yote$Mortality=="1", as.character(yote$MortDate), as.character(Sys.Date()))

  uni<-unique(yote$Serial)
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

  f<-f[f$Date >= startdates[1] & f$Date <= enddates[1] | f$Date >= startdates[2] & f$Date <= enddates[2]
       | f$Date >= startdates[3] & f$Date <= enddates[3],]
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
    Yote$Year<-strftime(Yote$TelemDate, format = "%Y")
    uni<-unique(Yote$Year)
    
    YoteMR<-data.frame()
    for(k in 1:length(uni)){
    sub<-Yote[Yote$Year == uni[k],]
    sub<-sub[order(sub$TelemDate),]
    subMR<-as.ltraj(xy = sub[, c("Easting", "Northing")], date = sub$TelemDate, id = sub$AID)

    subMR<-rbindlist(subMR, idcol = "id")
    
    YoteMR<-rbind(subMR, YoteMR)
    }
    ### Bind back to all coyote data ###

    Yote<-Yote[order(Yote$AID, Yote$TelemDate),]
    YoteMR<-YoteMR[order(YoteMR$id),]
    
    Yote<-merge(Yote, YoteMR, by.x = c('AID', 'TelemDate'), by.y = c('id', 'date'), keep.all = T)
    Yote$dist<-Yote$dist/1000 ### transforms to km

    s<-data.frame()
    Yote$AIDYr<-paste(Yote$AID, Yote$Year, sep = "_")
    uni<-unique(Yote$AIDYr)
    for(i in 1:length(uni)){
      sub<-Yote[Yote$AIDYr == uni[i],]

      for(k in 1:nrow(sub)){
        sub$TimeDiff[k]<-difftime(sub$TelemDate[k+1], sub$TelemDate[k], units = "hours")
        sub$HrMR[k]<-sub$dist[k]/sub$TimeDiff[k]

        print(k)

      }
      s<-rbind(sub, s)
      quant<-quantile(s$HrMR, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95, 0.99, 1), na.rm=T)
      quant<-as.numeric(quant[9])
      s<-s[s$HrMR < quant, ]
    }
    s$Hour<-strftime(s$TelemDate, format = "%H")
    s$Hour<-as.numeric(s$Hour)
    agg<-aggregate(s$HrMR, by=list(s$AIDYr, s$Hour), FUN = mean, na.rm=T)
    agg2<-aggregate(agg$x, by = list(agg$Group.2), FUN = mean, na.rm=T)

    quant<-quantile(agg$x, c(0.01, 0.05, 0.1, 0.25, 0.5,0.7, 0.75, 0.95, 0.99, 1), na.rm=T)
    quant<-as.numeric(quant[5])

    plot(agg2$Group.1, agg2$x, main = "Movement Rates of Coyotes", type = "l", ylim = c(0.3, 1.0))
    abline(h = quant, col = "red")


    s$act.cat<-ifelse(s$Hour >= 4 & s$Hour <= 8 | s$Hour >= 20 & s$Hour <= 23, "High", "Low")

    return(s)
    }







