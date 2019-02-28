#' @title Process all spatial data for analyses
#' @description Clip, re-project, and stack rasters for analyes
#' @param raspath path to rasters already clipped to study area
#' @param sagepath path to location of sage rasters
#' @param studypath path to location of study area polygon
#' @return Returns a raster stack with all spatial data cropped to large study area
#' @keywords spatial data, raster, stack
#' @export

ProcessSpatial<-function(raspath,sagepath, studypath){
  library(raster)
  library(rgdal)

  files<-unzip(raspath, files = NULL)
  files<-grep(".img$", files, value = TRUE)

  r<-list()
  for(i in 1:length(files)){
    tempras<-raster(files[i])
    r[[i]]<-assign(names(tempras), tempras)

    stack<-lapply(r, stack)
    all.stack<-stack(stack)
  }

  files<-unzip(sagepath, files = NULL)
  files<-grep(".img$", files, value = TRUE)

  r<-list()
  for(i in 1:length(files)){
    tempras<-raster(files[i])
    r[[i]]<-assign(names(tempras), tempras)

    stack<-lapply(r, stack)
    sage.stack<-stack(stack)
  }

  study<-readOGR(studypath)
  proj<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  study<-spTransform(study, CRS(proj))

beginCluster(n = 2)
all.stack<-projectRaster(from = all.stack, crs = proj, progress = "text")
sage.stack<-projectRaster(from = sage.stack, to = all.stack, progress = "text")
#sage.stack<-crop(sage.stack, study)
endCluster()

whole.stac<-stack(all.stack, sage.stack)
###################################################################
###### Build roads rasters ############

### Primary Roads ###
wyrd<-readOGR(dsn = "C:/Users/khuggler/Box Sync/DEER/Data/SpatialData/Vector/Roads/PrimaryRoads/Wyoming/WyPrimRoads.shp")
utrd<-readOGR(dsn = "C:/Users/khuggler/Box Sync/DEER/Data/SpatialData/Vector/Roads/PrimaryRoads/Utah/UTPrimRoads.shp")
cord<-readOGR(dsn = "C:/Users/khuggler/Box Sync/DEER/Data/SpatialData/Vector/Roads/PrimaryRoads/Colorado/COPrimRoads.shp")

proad<-do.call(rbind, c(wyrd, utrd, cord))

study<-spTransform(study, proj4string(proad))
proad<-crop(proad, study)

empty.ras<-whole.stac[[1]]
p<-empty.ras
p[]<-NA
proj<-"+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"

beginCluster(n=2)
p<-projectRaster(p, crs = proj, progress="text")
endCluster()

beginCluster(n=2)
proad<-spTransform(proad, proj4string(p))
proads<-rasterize(proad, p, field = 1, progress="text")
proaddist<-distance(proads, progress="text")
endCluster()

beginCluster(n=2)
prim.road<-projectRaster(from = proaddist, to = whole.stac, progress = "text")
endCluster()

whole.stack<-stack(whole.stac, prim.road)
names(whole.stack)[[14]]<-'Dist2PrimRoads'
writeRaster(prim.road, 'C:/Users/khuggler/Box Sync/DEER/Data/SpatialData/Rasters/Dist2PrimRoads.img', format = "HFA", overwrite = TRUE)

### Secondary Roads ###

wyrd2<-readOGR('C:/Users/khuggler/Box Sync/DEER/Data/SpatialData/Vector/Roads/SecondaryRoads/Wyoming/WySecRoads.shp')
utrd2<-readOGR('C:/Users/khuggler/Box Sync/DEER/Data/SpatialData/Vector/Roads/SecondaryRoads/Utah/DuchesneRoads.shp')
utrd3<-readOGR('C:/Users/khuggler/Box Sync/DEER/Data/SpatialData/Vector/Roads/SecondaryRoads/Utah/SummitRoads.shp')
utrd4<-readOGR('C:/Users/khuggler/Box Sync/DEER/Data/SpatialData/Vector/Roads/SecondaryRoads/Utah/UintahRoads.shp')
cord2<-readOGR('C:/Users/khuggler/Box Sync/DEER/Data/SpatialData/Vector/Roads/SecondaryRoads/Colorado/CoPrimRoads.shp')


secroad<-do.call(rbind, c(wyrd2, utrd2, utrd3, utrd4, cord2))
study<-spTransform(study, proj4string(secroad))
secroad<-crop(secroad, study)

empty.ras<-whole.stack[[1]]
p<-empty.ras
p[]<-NA
proj<-"+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"

beginCluster(n=2)
p<-projectRaster(p, crs = proj, progress="text")
endCluster()

beginCluster(n=2)
secroad<-spTransform(secroad, proj4string(p))
secroads<-rasterize(secroad, p, field = 1, progress="text")
secroaddist<-distance(secroads, progress="text")
endCluster()

beginCluster(n=2)
sec.road<-projectRaster(from = secroaddist, to = whole.stack, progress = "text")
endCluster()

whole.stack<-stack(whole.stack, sec.road)
names(whole.stack)[[15]]<-'Dist2SecRoads'
writeRaster(sec.road, 'C:/Users/khuggler/Box Sync/DEER/Data/SpatialData/Rasters/Dist2SecRoads.img', format = "HFA", overwrite = TRUE)


writeRaster(whole.stack, filename = "C:/Users/khuggler/Box Sync/DEER/Data/SpatialData/Rasters/RasterStack.img", format = "HFA",
          bylayer = TRUE, suffix = names(whole.stack), progress = "text", overwrite =TRUE)

return(whole.stack)
}

