#' @title Append AID to GPS data and subset to desired time period
#' @description Merge raw GPS Data with Animal IDs, and Sex
#' @param filepath path to all lion movement file
#' @param startdates vector of start dates to subset data
#' @param enddates vector of end dates to subset data
#' @return Returns a data.frame with all gps data, AnimalID, Sex, and Movement Rates
#' @keywords mountain lion, animal id, movement rate
#' @export
#' @examples
#' \donttest{Lion<-LionData(filepath = yourfile startdates = '2017-05-01', enddates = '2017-09-01')}


LionData<-function(filepath, startdates, enddates){
library(rbindlist)
lion<-read.csv(filepath, stringsAsFactors = F)
names(lion)[10:11]<-c('Easting', 'Northing')
lion<-lion[lion$AID != "BJ3029",]

library(adehabitatLT)
## Tranform to UTMs ##

all_lion<-lion[complete.cases(lion$Easting),]
coordinates(all_lion) <- c("Easting", "Northing")
proj<-'+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
proj4string(all_lion)<-proj

all_lion<-data.frame(all_lion)
all_lion$TelemDate<-as.POSIXct(all_lion$TelemDate, format = "%Y-%m-%d %H:%M")
all_lion$Date<-as.Date(all_lion$TelemDate, format = "%Y-%m-%d", tz = "MST")
all_lion<-all_lion[!duplicated(all_lion[c("AID", "TelemDate")]),] ### remove duplicated fixes

all_lion<-all_lion[all_lion$Date >= startdates[1] & all_lion$Date <= enddates[1] | all_lion$Date >= startdates[2] & all_lion$Date <= enddates[2] | all_lion$Date >= startdates[3] & all_lion$Date <= enddates[3],]



#all_lion<-data.frame(all_lion)
all_lion$Year<-strftime(all_lion$TelemDate, format = "%Y")
all_lion<-all_lion[complete.cases(all_lion$Year),]

all_lion$aid.yr<-paste(all_lion$AID, all_lion$Year, sep = "_")

#all_lion$AID<-as.character(all_lion$AID)
uni<-unique(all_lion$aid.yr)

all.traj<-NULL
for(i in 1:length(uni)){
  tmp<-all_lion[all_lion$aid.yr == uni[i],]
  tmp<-tmp[!duplicated(tmp$TelemDate),]

  temp.traj<-as.ltraj(data.frame(tmp$Easting, tmp$Northing), tmp$TelemDate, id = uni[i])
  id<-attr(temp.traj[[1]], which = "id")
  temp.traj<-data.frame(rbindlist(temp.traj, idcol = "id"))
  temp.traj$id<-id

  all.traj<-rbind(temp.traj, all.traj)

}


all_lion<-all_lion[order(all_lion$aid.yr, all_lion$TelemDate),]
LionMR<-all.traj[order(all.traj$id, all.traj$date),]

Lion<-merge(all_lion, LionMR, by.x = c('aid.yr', 'TelemDate'), by.y = c('id', 'date'), keep.all = T)

Lion$dist<-Lion$dist/1000 ### transforms to km

s<-data.frame()
uni<-unique(Lion$aid.yr)
for(i in 1:length(uni)){
  sub<-Lion[Lion$aid.yr == uni[i],]

  sub$movedist<-NA
  sub$timediff<-NA
  sub$moverate<-NA

  for(k in 2:nrow(sub)){
    sub$movedist[k]<-sqrt(((sub$Easting[k]-sub$Easting[k-1]))^2)+ sqrt((sub$Northing[k]-sub$Northing[k-1])^2)
    sub$timediff[k]<-(as.numeric(difftime(sub$TelemDate[k],sub$TelemDate[k-1]), units = "hours"))
    sub$moverate[k]<-ifelse(sub$movedist[k]==0, 0, (sub$movedist[k])/(as.numeric(sub$TelemDate[k]-sub$TelemDate[k-1])))
  }
  s<-rbind(sub, s)
  quant<-quantile(s$moverate, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95, 0.99, 1), na.rm=T)
  quant<-as.numeric(quant[8])

  #print(quant)
  print(i)

  s<-s[s$moverate <= quant, ]
}

s$Hour<-strftime(s$TelemDate, format = "%H")
s$Hour<-as.numeric(s$Hour)
agg<-aggregate(s$moverate, by=list(s$aid.yr, s$Hour), FUN = mean, na.rm=T)
agg2<-aggregate(agg$x, by = list(agg$Group.2), FUN = mean, na.rm=T)

quant<-quantile(agg$x, c(0.01, 0.05, 0.1, 0.25, 0.5,0.7, 0.75, 0.95, 0.99, 1), na.rm=T)
quant<-as.numeric(quant[5])

plot(agg2$Group.1, agg2$x, type = "l")
abline(h = quant, col = "red")


s$act.cat<-ifelse(s$Hour >= 0 & s$Hour <= 6 | s$Hour >= 17 & s$Hour <= 23, "High", "Low")

return(s)
}
