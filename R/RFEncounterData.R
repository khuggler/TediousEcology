#' @title Stack and extract all spatial data to elk, coyotes, and mountain lions (RF Encounter)
#' @description Stack rasters and extract points data for elk, coyotes, and mountain lions
#' @param elkdata object of data.frame of elk GPS data
#' @param liondata object of data.frame of lion GPS data
#' @param coyotedata object of data.frame of coyote GPS data
#' @param raspath path to raster folder
#' @param studypath path to study area polygon (where you want to define availability)
#' @return Returns a list object with RFData necessary to predict probably of use in RF models (elk, coyotes, and mountain lions)
#' @keywords elk, coyote, mountain lion, random forest, extract, raster, sample
#' @export


RFEncounterData<-function(elkdata, liondata, coyotedata, raspath, studypath){
library(raster)
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

elk<-elkdata[complete.cases(elkdata$Easting),]
random<-spsample(study, nrow(elk), type = "random")
random.ex<-data.frame(extract(rasstack, random))
random.ex$AID<-elk$AID
random.ex$Used<-0


coordinates(elk)<-c('Easting', 'Northing')
proj4string(elk)<-'+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
elk<-spTransform(elk, proj4string(rasstack))

used.ex<-data.frame(extract(rasstack, elk))
used.ex$AID<-elk$AID

used.ex$Used<-1

elkrf<-rbind(random.ex, used.ex)

random<-spsample(study, nrow(liondata), type = "random")
random.ex<-data.frame(extract(rasstack, random))
random.ex$AID<-liondata$AID
random.ex$Used<-0

lion<-liondata[complete.cases(liondata$Easting),]
coordinates(lion)<-c('Easting', 'Northing')
proj4string(lion)<-'+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
lion<-spTransform(lion, proj4string(rasstack))

used.ex<-data.frame(extract(rasstack, lion))
used.ex$AID<-lion$AID

used.ex$Used<-1

lionrf<-rbind(random.ex, used.ex)


random<-spsample(study, nrow(coyotedata), type = "random")
random.ex<-data.frame(extract(rasstack, random))
random.ex$AID<-coyotedata$AID
random.ex$Used<-0

coordinates(coyotedata)<-c('Easting', 'Northing')
proj4string(coyotedata)<-'+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
yote<-spTransform(coyotedata, proj4string(rasstack))

used.ex<-data.frame(extract(rasstack, yote))
used.ex$AID<-yote$AID

used.ex$Used<-1

coyoterf<-rbind(random.ex, used.ex)

rfdata<-list(lionrf, elkrf, coyoterf)

return(rfdata)

}
