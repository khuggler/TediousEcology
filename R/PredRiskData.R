#' @title BuildRF model of predation risk
#' @description Build random forest model to predict probability of kill
#' @param clustpath path to cluster data
#' @param subset Logical TRUE/FALSE. Whether or not data should be subset to a certaind date range
#' @param startdates vector of desired start dates
#' @param enddates vector of desired end dates
#' @param raspath path to raster stack
#' @param studypath path to study area polygon (where you want to define availability)
#' @param nsamps number of random samples per 1 used point
#' @param pathout path to write csv of predation risk data
#' @return Returns a data.frame object with RFData necessary to predict probability of kill in RF models mountain lions only
#' @keywords mountain lion, predation risk, deer
#' @export

PredRiskData<-function(clustpath, subset, startdates, enddates,raspath, studypath, nsamps, pathout){
  clust<-read.csv(clustpath, stringsAsFactors = F)
  clust$Date.<-as.Date(clust$Date., format = "%m/%d/%Y")

  if(subset == TRUE){
  clust<-clust[clust$Date. >= startdates[1] & clust$Date. <= enddates[1] |
               clust$Date. >= startdates[2] & clust$Date. <= enddates[2],]
  clust<-clust[clust$Cluster.Description == "KILL" & clust$Species == "MULE DEER",]
  }

  clust<-clust[, c(5,14,18,19)]

  names(clust)<-c('Date', 'Sex', 'Easting', 'Northing')


  deer<-read.csv(dbpath, stringsAsFactors = F)
  deer$MortalityDate<-as.Date(deer$MortalityDate, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))
  cause<-c('Lion', "Lion ", "Predation")
  deer<-deer[deer$X %in% cause,]
  deer<-deer[deer$Species == "MD",]

  if(subset == TRUE){
  deer<-deer[deer$MortalityDate >= startdates[1] & deer$MortalityDate <= enddates[1] |
              deer$MortalityDate >= startdates[2] & deer$MortalityDate <= enddates[2],]
  }

  deer<-deer[, c(47, 26, 48, 49)]
  names(deer)<-c('Date', 'Sex', 'Easting', 'Northing')

  allkill<-rbind(clust, deer)
  allkill$Month<-as.numeric(strftime(allkill$Date, format = "%m"))
  allkill$Season<-ifelse(allkill$Month >= 5 & allkill$Month <= 10, "Summer", "Winter")
  allkill$Month<-as.character(allkill$Month)
  allkill$Season<-as.character(allkill$Season)
################################################
##### sample rnadom locations #####
###############################################
  library(rgdal)
  files<-unzip(raspath, files = NULL)
  files<-grep(".img$", files, value = TRUE)

  r<-list()
  for(i in 1:length(files)){
    tempras<-raster(files[i])
    r[[i]]<-assign(names(tempras), tempras)

    stack<-lapply(r, stack)
    rasstack<-stack(stack)
  }

  study<-readOGR(studypath)
  study<-spTransform(study, proj4string(rasstack))

  allkill$Kill<-1
  allkill$ID<-1:nrow(allkill)
  allkill$Easting<-as.numeric(allkill$Easting)
  allkill$Northing<-as.numeric(allkill$Northing)
  allkill<-allkill[complete.cases(allkill$Easting),]

  coordinates(allkill)<-c('Easting', 'Northing')
  proj4string(allkill)<-"+proj=utm +zone=12 +ellps=WGS84 +units=m +no_defs"
  allkill<-spTransform(allkill, proj4string(rasstack))
  allkill<-data.frame(allkill)

  uni<-unique(allkill$ID)
  d<-data.frame()
  random<-data.frame()
  if(nsamps >1){
  for(i in 1:nrow(allkill)){
    sub<-allkill[i,]
    random<-spsample(study, n = nsamps, type = "random")
    random$Date<-rep(sub$Date, nsamps)
    random$Sex<-rep(sub$Sex, nsamps)
    random$Season<-rep(sub$Season, nsamps)
    random$Kill<-rep(0, nsamps)
    random$ID<-rep(sub$ID, nsamps)

    random<-data.frame(random)
    names(random)[6:7]<-c('Easting', 'Northing')
    d<-rbind(random, d)

  }
}

if(nsamps == 1){
  for(i in 1:nrow(allkill)){
    sub<-allkill[i,]
    random<-spsample(study, n = nsamps, type = "random")
    random$Date<-sub$Date
    random$Sex<-sub$Sex
    random$Season<-sub$Season
    random$Kill<-0
    random$ID<-sub$ID

    random<-data.frame(random)
    names(random)[6:7]<-c('Easting', 'Northing')
    d<-rbind(random, d)

  }
}
  d<-d[,c(1:7)]
  allkill<-allkill[,c(1,2,4,5,6,7,8)]
  names(allkill)[6:7]<-c('Easting', 'Northing')

  killdata<-rbind(d, allkill)

########################################################
  ## Extract Covariates ##
########################################################
  coordinates(killdata)<-c('Easting', 'Northing')
  proj4string(killdata)<-proj4string(rasstack)

  killex<-data.frame(extract(rasstack, killdata, type = "simple"))
  killdata<-data.frame(killdata)
  killdata<-cbind(killdata, killex)

write.csv(killdata, pathout, row.names = F)

return(killdata)

}
