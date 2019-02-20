#' @title Function to build mcps for coyotes, lions, and elk
#' @description Build minimum convex polygons (with buffer if needed)
#' @param deerdata data.frame of deer gps data with repro info
#' @param elkdata data.frame of elk gps data with MRs
#' @param liondata data.frame of lion gps data with MRs
#' @param coyotedata data.frame of coyote gps data with MRs
#' @param percent percent of points to be contained in MCP
#' @return Returns a data.frame with all deer gps data with repro status and activity of lions, elk, and coyotes
#' @keywords deer, gps, activity
#' @export
BuildMcps<-function(deerdata, liondata, elkdata, coyotedata, percent){
  library(adehabitatHR)
  deerdata<-deerdata[,c(3:4)]
  deerdata$Spp<-"MD"

  liondata<-liondata[,c(7:8)]
  liondata$Spp<-"Lion"
  names(liondata)[1:2]<-c('Latitude', 'Longitude')

  coordinates(elkdata)<-c('Easting', 'Northing')
  proj<-"+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0"
  proj4string(elkdata)<-proj
  elkdata<-spTransform(elkdata, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  elkdata<-data.frame(elkdata)
  names(elkdata)[24:23]<-c('Latitude', 'Longitude')
  elkdata<-elkdata[,c(24,23)]
  elkdata$Spp<-"Elk"

  coordinates(coyotedata)<-c('Easting', 'Northing')
  proj<-"+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0"
  proj4string(coyotedata)<-proj
  coyotedata<-spTransform(coyotedata, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  coyotedata<-data.frame(coyotedata)
  names(coyotedata)[25:24]<-c('Latitude', 'Longitude')
  coyotedata<-coyotedata[,c(25,24)]
  coyotedata$Spp<-"Yote"

  all<-rbind(deerdata, liondata)
  all<-rbind(all, elkdata)
  all<-rbind(all, coyotedata)

  coordinates(all)<-c('Longitude', 'Latitude')
  proj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  proj4string(all)<-proj
  all<-spTransform(all,"+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0")

  mcppolys <- mcp(all[,"Spp"], percent=percent, unin="m", unout="km2")
}
