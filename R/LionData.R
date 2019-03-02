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
all_lion$TelemDate<-as.POSIXct(all_lion$TelemDate, format = "%m/%d/%Y %H:%M", tz = "MST")
all_lion$Date<-as.Date(all_lion$TelemDate, format = "%Y-%m-%d", tz = "MST")
all_lion<-all_lion[!duplicated(all_lion[c("AID", "TelemDate")]),]
all_lion<-all_lion[all_lion$Date >= startdates[1] & all_lion$Date <= enddates[1] | all_lion$Date >= startdates[2] & all_lion$Date <= enddates[2],]


LionMR<-as.ltraj(xy = all_lion[, c("Easting", "Northing")], date = all_lion$TelemDate, id = all_lion$AID)
LionMR<-rbindlist(LionMR, idcol = "id")


all_lion<-all_lion[order(all_lion$AID),]
LionMR<-LionMR[order(LionMR$id),]
all_lion<-cbind(all_lion, LionMR[,c(5:11)])
all_lion$dist<-all_lion$dist/1000 ### transforms to km

s<-data.frame()
uni<-unique(all_lion$AID)
for(i in 1:length(uni)){
  sub<-all_lion[all_lion$AID == uni[i],]

  for(k in 1:nrow(sub)){
    sub$TimeDiff[k]<-difftime(sub$TelemDate[k+1], sub$TelemDate[k], units = "hours")
    sub$HrMR[k]<-sub$dist[k]/sub$TimeDiff[k]

    print(k)

  }
  s<-rbind(sub, s)
}

s$Hour<-strftime(s$TelemDate, format = "%H")
s$Hour<-as.numeric(s$Hour)
agg<-aggregate(s$HrMR, by=list(s$Hour), FUN = mean, na.rm=T)
plot(agg$Group.1, agg$x, type = "l")

quant<-quantile(s$HrMR, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.95, 0.99, 1), na.rm=T)
quant<-as.numeric(quant[6])

s$act.cat<-ifelse(s$HrMR >= quant, "High", "Low")


return(s)
}
