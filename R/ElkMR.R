#' @title Function to calculate movement rates of elk
#' @description Merge cleaned GPS data with the movement rate of elk
#' @param gps data.frame of gps data
#' @param startdates vector (date format) of the start dates to subset GPS data
#' @param enddates vector (date format) of the end dates to subset GPS data
#' @param subspp what species to subset
#' @param subsex what sex to subset
#' @return Returns a data.frame with all gps data, AnimalID, Sex, and Species, and Reproductive Status (Repro or Non-Repro)
#' @keywords neonate, gps, reproductive
#' @export
#' @examples
#' \donttest{ElkData<-ElkMR(gps = yourgpsdata, startdates = c('2017-05-01', '2018-05-01'), enddates = c('2017-09-01', '2018-09-01'), subspp = "ELK", subsex = "F")}

ElkMR<-function(gps, startdates, enddates, subspp, subsex){
  ek<-gps[gps$Spp == subspp & gps$Sex == subsex,]
  ek<-ek[ek$Date >= startdates[1] & ek$Date <= enddates[1] | ek$Date >= startdates[2] & ek$Date <= enddates[2] | ek$Date >= startdates[3] & ek$Date <= enddates[3],]

  library(adehabitatLT)
  library(data.table)
  library(sp)
  ## Tranform to UTMs ##

  ek<-ek[complete.cases(ek$Longitude),]
  coordinates(ek) <- c("Longitude", "Latitude")
  proj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  proj4string(ek)<-proj
  ek<-sp::spTransform(ek, '+proj=utm +zone=12 +ellps=GRS80 +datum=WGS84 +units=m +no_defs')
  dimnames(ek@coords)[[2]]<- c('Easting', "Northing")

  ek<-data.frame(ek)
  ek$AID<-as.character(ek$AID)
  ek$Year<-strftime(ek$TelemDate, format = "%Y")
  
  uni<-unique(ek$Year)
  ElkMR<-data.frame()
  for(k in 1:length(uni)){
  sub.elk<-ek[ek$Year == uni[k],]
  subMR<-as.ltraj(xy = sub.elk[, c("Easting", "Northing")], date = sub.elk$TelemDate, id = sub.elk$AID)

  subMR<-rbindlist(subMR, idcol = "id")
  ElkMR<-rbind(subMR, ElkMR)
  }

  elk<-ek[order(ek$AID),]
  ElkMR<-ElkMR[order(ElkMR$id),]
  
  elk<-merge(elk, ElkMR, by.x = c('AID', 'TelemDate'), by.y = c('id', 'date'), keep.all = TRUE)
  elk$dist<-elk$dist/1000 ### transforms to km

  s<-data.frame()
  uni<-unique(elk$AID)
  for(i in 1:length(uni)){
    sub<-elk[elk$AID == uni[i],]

    for(k in 1:nrow(sub)){
      sub$TimeDiff[k]<-difftime(sub$TelemDate[k+1], sub$TelemDate[k], units = "hours")
      sub$HrMR[k]<-sub$dist[k]/sub$TimeDiff[k]

      print(k)

    }
    s<-rbind(sub, s)
    s<-rbind(sub, s)
    quant<-quantile(s$HrMR, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95, 0.99, 1), na.rm=T)
    quant<-as.numeric(quant[9])
    s<-s[s$HrMR < quant, ]

  }

  s$Hour<-strftime(s$TelemDate, format = "%H")
  s$Hour<-as.numeric(s$Hour)
  agg<-aggregate(s$HrMR, by=list(s$AID, s$Hour), FUN = mean, na.rm=T)
  agg2<-aggregate(agg$x, by = list(agg$Group.2), FUN = mean, na.rm=T)

  quant<-quantile(agg$x, c(0.01, 0.05, 0.1, 0.25, 0.5,0.7, 0.75, 0.95, 0.99, 1), na.rm=T)
  quant<-as.numeric(quant[5])

  plot(agg2$Group.1, agg2$x, type = "l")
  abline(h = quant, col = "red")


  s$act.cat<-ifelse(s$Hour >= 4 & s$Hour <= 8 | s$Hour >= 17 & s$Hour <= 22, "High", "Low")


  return(s)
  }
