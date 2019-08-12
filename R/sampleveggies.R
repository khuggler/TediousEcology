#' @title Sample random points from neonates for vegetation work
#' @description Sample random points from neonates for vegetation work
#' @param x path to neonate data
#' @param a list of time intervals to sample (in days)
#' @param year year of interest to subset neonate data
#' @keywords vegetation, sample, gps, neonate
#' @export

#' @title Sample random points from neonates for vegetation work
#' @description Sample random points from neonates for vegetation work
#' @param x path to neonate data
#' @param a list of time intervals to sample (in days)
#' @param year year of interest to subset neonate data
#' @keywords vegetation, sample, gps, neonate
#' @export


sampleveggies<-function(x, a, year){
  library(dplyr)
  library(geosphere)
  library(sp)

  ## get start and endates in the right format for working with ##
  neo<-read.csv(x, stringsAsFactors = F)
  neo$StartDate<-as.Date(neo$StartDate, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))
  neo$year<-strftime(neo$StartDate, format = "%Y")
  neo<-neo[neo$year == year,]
  currentdate<-as.character(Sys.Date())
  currentdate<-strftime(currentdate, format = "%m/%d/%Y")
  neo$EndDate<-ifelse(neo$EndDate == "", currentdate, neo$EndDate)
  neo$EndDate<-as.Date(neo$EndDate, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))

  ## calculate time alive for each litter! ##

  neo$diffdays<-as.numeric(difftime(neo$EndDate, neo$StartDate, units = "days"))

  out<-list()
  for(i in 1:length(a)){
    timeint<-a[[i]]
    neoids<-neo[neo$diffdays >= max(timeint),]$FawnID

    neosub<-neo[neo$FawnID %in% neoids,]

    #testagg<-as.data.frame(table(xSub$MomSerial))
    #testagg<-testagg[testagg$Freq >=3,]
    #if(is.null(nrow(testagg))){
    #  out[[i]]<-list(NULL, NULL)
    #  next
    #}

    ## get neonate GPS data ##

    neogps<-Part::getVec('C:/Users/Public/Documents/VAS/GPS Plus X/AutoExport/')

    neodf<-data.frame(neogps)

    new<-neodf[neodf$CollarSerialNumber %in% unique(neosub$FawnID),]
    names(new)[1]<-'FawnID'

    ## merge mom serial information into neonate gps data ##
    new.df<-merge(new, neosub, by = "FawnID", all.x = TRUE)

    ## now sample gps data from interval of choice ##
    maxtime<-max(timeint)
    mintime<-min(timeint)
    uni<-unique(new.df$MomSerial)

    oats<-data.frame()
    for(k in 1:length(uni)){
      sub<-new.df[new.df$MomSerial == uni[k],]
      start<-sub$StartDate[1] + mintime
      end<-sub$StartDate[1] + maxtime -1
      sub$FixDate<-strftime(sub$TelemDate, format = "%Y-%m-%d")
      sub<-sub[sub$FixDate >= start & sub$FixDate <= end,]

      samps<-as.data.frame(sub %>% sample_n(size=3))

      oats<-rbind(oats, samps)
    }

    oats$Interval<-names(a)
    oats$Used<-1

    oats$UseBearing<-runif(nrow(oats), 0,360)
    oats$RandBearing<-runif(nrow(oats), 0,360)
    oats$RandDist<-runif(nrow(oats), 50,100)
    oats$RandDat<-runif(nrow(oats), 0,360)
    oats$v<-rep(1:3, length(unique(oats$MomSerial)))
    oats$UseID<-paste0(oats$MomSerial, "_", oats$Interval, "_",oats$v,"U")
    #agg<-aggregate(samps$CollarSerialNumber, by=list(samps$MomSerial), FUN=function(x) length(unique(x)))
    #agg$Interval<-3
    #samps$Interval<-3

    uni<-unique(oats$MomSerial)

    allsamps<-data.frame()
    for(l in 1:length(uni)){
      sub<-oats[oats$MomSerial == uni[l],]
      sub<-sub[c('FawnID', 'MomSerial', 'Lat', 'Long', 'Interval', 'Used', 'UseBearing', 'RandBearing', 'RandDist', 'v', 'UseID')]

      sp::coordinates(sub)<-~Long +Lat
      sp::proj4string(sub)<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


      Rand<-geosphere::destPoint(sub, sub@data$RandBearing, sub@data$RandDist)
      Rand<-data.frame(Rand)
      names(Rand)<-c('Long', 'Lat')

      sub<-data.frame(sub)
      Rand$FawnID<-sub$FawnID
      Rand$MomSerial<-sub$MomSerial
      Rand$Interval<-names(a)
      Rand$Used<-0
      Rand$UseID<-paste0(Rand$MomSerial, "_", Rand$Interval, "_",Rand$v,"A")

      Rand<-Rand[,c('FawnID', 'MomSerial', 'Interval', 'Used', 'v', 'UseID', 'Long', 'Lat')]

      sub<-sub[,c('FawnID', 'MomSerial', 'Interval', 'Used', 'v', 'UseID', 'Long', 'Lat')]

      new<-rbind(sub, Rand)

      allsamps<-rbind(new, allsamps)
      allsamps$UseID<-as.character(allsamps$UseID)
    }


  }
  allsamps$Bearing<-runif(nrow(allsamps), 0,360)
  return(allsamps)
}

