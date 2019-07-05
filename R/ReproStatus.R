#' @title Function to append Reproductive Status of an animal to at each fix of GPS data
#' @description Merge cleaned GPS data with the reproductive status of animals at each fix
#' @param gps data.frame of gps data
#' @param startdates vector (date format) of the start dates to subset GPS data
#' @param enddates vector (date format) of the end dates to subset GPS data
#' @param neodat path to neonate database
#' @param capdat path to capture database
#' @param subspp what species to subset
#' @param subsex what sex to subset
#' @param subset TRUE/FALSE. Whether or not function should remove non-repro animals
#' @return Returns a data.frame with all gps data, AnimalID, Sex, and Species, and Reproductive Status (Repro or Non-Repro)
#' @keywords neonate, gps, reproductive
#' @export
#' @examples
#' \donttest{ReproData<-ReproStatus(gps = yourgpsdata, startdates = c('2017-05-01', '2018-05-01'), enddates = c('2017-09-01', '2018-09-01'), neodat = yourpath, subspp = "MD", subsex = "F")}

ReproStatus<-function(gps, startdates, enddates, neodat,capdat, subspp, subsex, subset){

  sub<-gps[gps$Date >= startdates[1] & gps$Date <= enddates[1] | gps$Date >= startdates[2] & gps$Date <= enddates[2] |
             gps$Date >= startdates[3] & gps$Date <= enddates[3],]
  sub<-sub[sub$Spp %in% subspp & sub$Sex %in% subsex,]
  sub$Year<-strftime(sub$Date, format = "%Y")
  sub$AIDYr<-paste(sub$AID, sub$Year, sep = "_")

  neo<-read.csv(neodat, stringsAsFactors = F)
  neo$StartDate<-as.Date(neo$StartDate, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))
  p<-Sys.Date()
  neo$EndDate<-as.Date(neo$EndDate, format = "%m/%d/%Y")
  neo$EndDate<-ifelse(is.na(neo$EndDate), as.character(p), as.character(neo$EndDate))
  neo$EndDate<-as.Date(neo$EndDate, format = "%Y-%m-%d")
  neo$Year<-strftime(neo$StartDate, format = "%Y")
  neo$AIDYr<-paste(neo$MomAID, neo$Year, sep = "_")

  cap<-read.csv(capdat, stringsAsFactors = F) ######
  cap$CaptureDate<-as.Date(cap$CaptureDate, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))
  cap$Month<-strftime(cap$CaptureDate, format = "%m")
  cap$Year<-strftime(cap$CaptureDate, format = "%Y")
  cap$MoYr<-paste(cap$Month, cap$Year, sep = "_")

  cap<-cap[cap$MoYr == "04_2017" | cap$MoYr == "04_2018" | cap$MoYr == "04_2019",]
  cap$AIDYr<-paste(cap$UAID, cap$Year, sep = "_")

  uni<-unique(sub$AIDYr)
  new.dat<-NULL

  for(z in 1:length(uni)){
    asub<-sub[sub$AIDYr == uni[z],]
    neosub<-neo[neo$AIDYr == uni[z],]
    capsub<-cap[cap$AIDYr == uni[z],]

    if(nrow(capsub) == 0){
      capsub<-data.frame(Number.Fetus = NA, Pregnant = NA)
    }

    if(nrow(neosub) > 1){
      end<-max(neosub$EndDate)
      start<-max(neosub$StartDate)-1
    }

    if(nrow(neosub) == 1){
      end<-neosub$EndDate
      start<-neosub$StartDate-1
    }

    if(nrow(neosub) == 0){
      end<-NA
      start<-NA
    }

    asub$FetusNumber<-capsub$Number.Fetus
    asub$PregStat<-capsub$Pregnant
    asub$AllCatch<-ifelse(nrow(neosub) < capsub$Number.Fetus,0,1)
    asub$NeoCatch<-ifelse(nrow(neosub)> 0, 1, 0)
    asub$ReproStatus <- ifelse(end < asub$Date, "NonRepro", "Repro")
    asub$ReproStatus<-ifelse(asub$PregStat == 0, "NonRepro", asub$ReproStatus)
    asub$Keep<-ifelse(asub$AllCatch == 0 & asub$ReproStatus == "NonRepro", 0, 1)
    asub$birth.date<-start
    asub$days.to.part<-as.numeric(difftime(asub$Date, start, units = "days"))

    if(subset == TRUE){
    asub<-asub[asub$Keep ==1,]
    }

    new.dat<-rbind(new.dat, asub)


    print(z)

  }

  return(new.dat)
}
