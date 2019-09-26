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
  
  ek$aid.yr<-paste(ek$AID, ek$Year, sep = "_")
  
  all.traj<-NULL
  uni<-unique(ek$aid.yr)
  for(i in 1:length(uni)){
    tmp<-ek[ek$aid.yr == uni[i],]
    tmp<-tmp[!duplicated(tmp$TelemDate),]
    
    temp.traj<-as.ltraj(data.frame(tmp$Easting, tmp$Northing), tmp$TelemDate, id = uni[i])
    id<-attr(temp.traj[[1]], which = "id")
    temp.traj<-data.frame(rbindlist(temp.traj, idcol = "id"))
    temp.traj$id<-id
    
    all.traj<-rbind(temp.traj, all.traj)
    
  }
  
  Elk<-ek[order(ek$aid.yr, ek$TelemDate),]
  ElkMR<-all.traj[order(all.traj$id, all.traj$date),]
  
  Elk<-merge(Elk, ElkMR, by.x = c('aid.yr', 'TelemDate'), by.y = c('id', 'date'), keep.all = T)
  
  Elk$dist<-Elk$dist/1000 ### transforms to km

  elk<-Elk

  s<-data.frame()
  uni<-unique(elk$AID)
  for(i in 1:length(uni)){
    sub<-elk[elk$AID == uni[i],]
    
    for(k in 1:nrow(sub)){
      sub$TimeDiff[k]<-difftime(sub$TelemDate[k+1], sub$TelemDate[k], units = "hours")
      sub$HrMR[k]<-sub$dist[k]/sub$TimeDiff[k]
      
      #print(k)
      
    }
    s<-rbind(sub, s)
    quants<-quantile(s$HrMR, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95, 0.99, 1), na.rm=T)
    quant<-as.numeric(quants[8])
    #out<-as.numeric(quants[9])
    print(quant)
    print(i)
    s<-s[s$HrMR <= quant, ]
  }
  s$Hour<-strftime(s$TelemDate, format = "%H")
  s$Hour<-as.numeric(s$Hour)
  agg<-aggregate(s$HrMR, by=list(s$AID, s$Hour), FUN = mean, na.rm=T)
  agg2<-aggregate(agg$x, by = list(agg$Group.2), FUN = mean, na.rm=T)
  
  quant<-quantile(agg$x, c(0.01, 0.05, 0.1, 0.25, 0.5,0.7, 0.75, 0.95, 0.99, 1), na.rm=T)
  quant<-as.numeric(quant[5])
  
  plot(agg2$Group.1, agg2$x, main = "Movement Rates of Elk", type = "l")
  abline(h = quant, col = "red")
  
  
  s$act.cat<-ifelse(s$Hour >= 5 & s$Hour <= 7 | s$Hour >= 17 & s$Hour <= 22, "High", "Low")
  
  return(s)
}

