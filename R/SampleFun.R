#' @title Sample Fun
#' @description Identify the number of sample points necessary to adequately represent variation in study area
#' @param raspath path to zip folder of rasters
#' @param nmin number of minimum available points to test
#' @param nmax number of maximum available points to test
#' @param by interval of number of points to try (10, 20, 100, etc.)
#' @param poly path to study area polygon
#' @param proj projection (character) of study area polygon
#' @return Returns a data.frame object with mean extracted values of each raster at each of the specified point intervals
#' @keywords available points, sampling, rsf
#' @export
SampleFun<-function(raspath, nmin, nmax, by, poly, proj){
  #for(i in 1:length(ras@layers)){
  #tempras<-ras[[i]]
  poly<-readOGR(poly)
  for(k in seq(nmin, nmax, by)){
    sp<-spsample(poly, k, type = "random")
    sp<-data.frame(sp)
    names(sp)[1]<-"x"
    names(sp)[2]<-"y"
    samps<-sp
    coordinates(samps)<-~ x+ y
    proj4string(samps)<-proj4string(poly)


    files<-unzip(raspath, files = NULL)
    files<-grep(".img$", files, value = TRUE)

    r<-list()
    for(i in 1:length(files)){
      tempras<-raster(files[i])
      r[[i]]<-assign(names(tempras), tempras)

      stack<-lapply(r, stack)
      ras<-stack(stack)
    }


    a<-data.frame()
    for(i in 1:length(ras@layers)){
      tempras<-ras[[i]]
      samps<-spTransform(samps, proj4string(tempras))
      sub<-data.frame(extract(tempras, samps))
      #avg<-mean(sub[,1], na.rm=T)
      names(sub)<-names(ras[[i]])
      avg<-mean(sub[,1], na.rm=T)
      avg<-data.frame(Mean = avg, Name = names(sub), N = k)

      a<-rbind(avg, a)
    }
    if(k == nmin){
      allavg<-a
    }
    if(k > nmin & k <= nmax){
      allavg<-rbind(allavg, a)
    }
  }
  return(allavg)
}
