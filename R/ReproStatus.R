#' @title Function to append Reproductive Status of an animal to at each fix of GPS data
#' @description Merge cleaned GPS data with the reproductive status of animals at each fix
#' @param gps data.frame of gps data
#' @param startdates vector (date format) of the start dates to subset GPS data
#' @param endates vector (date format) of the end dates to subset GPS data
#' @param neodat path to neonate database
#' @param subspp what species to subset
#' @param subsex what sex to subset
#' @return Returns a data.frame with all gps data, AnimalID, Sex, and Species, and Reproductive Status (Repro or Non-Repro)
#' @keywords neonate, gps, reproductive
#' @export
#' @examples
#' \donttest{ReproData<-ReproStatus(gps = yourgpsdata, startdates = c('2017-05-01', '2018-05-01'), enddates = c('2017-09-01', '2018-09-01'), neodat = yourpath, subspp = "MD", subsex = "F")}

ReproStatus<-function(gps, startdates, enddates, neodat, subspp, subsex){

  sub<-gps[gps$Date >= startdates[1] & gps$Date <= enddates[1] | gps$Date >= startdates[2] & gps$Date <= enddates[2],]
  sub<-sub[sub$Spp %in% subspp & sub$Sex %in% subsex,]
  sub$Year<-strftime(sub$Date, format = "%Y")
  sub$AIDYr<-paste(sub$AID, sub$Year, sep = "_")

  neo<-read.csv(neodat, stringsAsFactors = F)
  neo$StartDate<-as.Date(neo$StartDate, format = "%Y-%m-%d")
  p<-Sys.Date()
  neo$EndDate<-as.Date(neo$EndDate, format = "%m/%d/%Y")
  neo$EndDate<-ifelse(is.na(neo$EndDate), as.character(p), as.character(neo$EndDate))
  neo$EndDate<-as.Date(neo$EndDate, format = "%Y-%m-%d")
  neo$Year<-strftime(neo$StartDate, format = "%Y")
  neo$AIDYr<-paste(neo$MomAID, neo$Year, sep = "_")


  uni<-unique(sub$AIDYr)

  new.dat<-NULL

  for(z in 1:length(uni)){
    asub<-sub[sub$AIDYr == uni[z],]
    neosub<-neo[neo$AIDYr == uni[z],]

    if(nrow(neosub) > 1){
      end<-max(neosub$EndDate)
    }

    if(nrow(neosub) == 1){
      end<-neosub$EndDate
    }

    if(nrow(neosub) == 0){
      end<-NA
    }

    asub$ReproStatus <- ifelse(end < asub$Date, "NonRepro", "Repro")

    new.dat<-rbind(new.dat, asub)

    print(z)

  }

  return(new.dat)
}
