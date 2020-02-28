#' @title Read netCDF files, crop, and stack into climate raster
#' @description Read in netCDF files and manipulate to fit study area
#' @param root.dir path of root directory where spatial data is located 
#' @param year year to download (must do one at a time)
#' @param study path to study area shapefile
#' @return Returns a raster brick with all climate data for specific year
#' @keywords climate, pdsi, temperature, swe
#' @export
#' @examples
#' 

climate.dat<-function(root.dir, year, study){
  
  new.stack<-list()
  paths<-paste0(root.dir, year, "/")
  files<-list.files(paths, pattern = ".nc")
  study<-readOGR(study)
  
  for(i in 1:length(files)){
    temp<-raster::stack(paste0(paths, files[i]))
    temp<-temp[[5:9]]
   
    months<-c('May', 'June', 'July', 'August', 'September')
    names(temp)<-paste0(months, year)
    
    study<-spTransform(study, proj4string(temp))
    
    temp<-raster::crop(temp, study)
    
    new.stack[[i]]<-temp
    names(new.stack)[[i]]<-files[i]
  }
    return(new.stack)
}