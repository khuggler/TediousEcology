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
#' \donttest{yote.gps<-yote.gps(username = yourusername, password = yourpassword, dirdown = tempdir, cType = "ATS/IRID", yotedat = path, startdates = '2017-05-01', enddates = '2017-09-01')}


yote.moverates<-function(username, password,dirdown, cType = "ATS/IRID", yotedat, startdates, enddates){
  
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
  Yote$aid.yr<-paste(Yote$AID, Yote$Year, sep = "_")
  
  all.traj<-NULL
  uni<-unique(Yote$aid.yr)
  for(i in 1:length(uni)){
    tmp<-Yote[Yote$aid.yr == uni[i],]
    tmp<-tmp[!duplicated(tmp$TelemDate),]
    
    temp.traj<-as.ltraj(data.frame(tmp$Easting, tmp$Northing), tmp$TelemDate, id = uni[i])
    id<-attr(temp.traj[[1]], which = "id")
    temp.traj<-data.frame(rbindlist(temp.traj, idcol = "id"))
    temp.traj$id<-id
    
    all.traj<-rbind(temp.traj, all.traj)
    
  }
  
  Yote<-Yote[order(Yote$aid.yr, Yote$TelemDate),]
  YoteMR<-all.traj[order(all.traj$id, all.traj$date),]
  
  Yote<-merge(Yote, YoteMR, by.x = c('aid.yr', 'TelemDate'), by.y = c('id', 'date'), keep.all = T)
  
  Yote$dist<-Yote$dist/1000 ### transforms to km
  
  Yote<-Yote[Yote$aid.yr != "1_2019",] ## remove this coyote
  
  s<-data.frame()
  uni<-unique(Yote$aid.yr)
  for(i in 1:length(uni)){
    sub<-Yote[Yote$aid.yr == uni[i],]
    
    sub$movedist<-NA
    sub$timediff<-NA
    sub$moverate<-NA
    
    for(k in 2:nrow(sub)){
      
      sub$movedist[k]<-sqrt(((sub$Easting[k]-sub$Easting[k-1]))^2)+ sqrt((sub$Northing[k]-sub$Northing[k-1])^2)
      sub$timediff[k]<-(as.numeric(difftime(sub$TelemDate[k],sub$TelemDate[k-1]), units = "hours"))
      sub$moverate[k]<-ifelse(sub$movedist[k]==0, 0, (sub$movedist[k])/(as.numeric(sub$TelemDate[k]-sub$TelemDate[k-1])))
      
    }
    s<-rbind(sub, s)
    
    quants<-quantile(s$moverate, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95, 0.99, 1), na.rm=T)
    quant<-as.numeric(quants[8]) ## remove crazy MRs
    #out<-as.numeric(quants[9])
    #print(quant)
    print(i)
    s<-s[s$moverate <= quant, ]
  }
  s$Hour<-strftime(s$TelemDate, format = "%H", tz = "MST")
  s$Hour<-as.numeric(s$Hour)
  agg<-aggregate(s$moverate, by=list(s$aid.yr, s$Hour), FUN = mean, na.rm=T)
  agg2<-aggregate(agg$x, by = list(agg$Group.2), FUN = mean, na.rm=T)
  
  quant<-quantile(agg$x, c(0.01, 0.05, 0.1, 0.25, 0.5,0.7, 0.75, 0.95, 0.99, 1), na.rm=T)
  quant<-as.numeric(quant[5]) ## 75th percentile
  
  plot(agg2$Group.1, agg2$x, main = "Movement Rates of Coyotes", type = "l")
  abline(h = quant, col = "red")
  
  
  s$act.cat<-ifelse(s$Hour >= 4 & s$Hour <= 8 | s$Hour >= 19 & s$Hour <= 23, "High", "Low")
  
  return(s)
}








