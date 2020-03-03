#' @title Read netCDF files, crop, and stack into climate raster
#' @description Read in netCDF files and manipulate to fit study area
#' @param root.dir path of root directory where spatial data is located 
#' @param study path to study area shapefile
#' @return Returns a list of raster bricks with all climate data
#' @keywords climate, pdsi, temperature, swe
#' @export
#' @examples
#' 

climate.dat<-function(root.dir,study){
  
  new.stack<-list()
  files<-list.files(root.dir, pattern = ".nc")
  paths<-paste0(root.dir, files)
  study<-rgdal::readOGR(study)
  
  years<-c('2016', '2017', '2018')
  for(i in 1:length(paths)){
    temp<-raster::stack(paths[i])
    temp<-temp[[4:9]]
   
    months<-c('April','May', 'June', 'July', 'August', 'September')
    names(temp)<-paste0(months, years[i])
    
    study<-spTransform(study, proj4string(temp))
    
    temp<-raster::crop(temp, study)
    
    new.stack[[i]]<-temp
    
  }
    return(new.stack)
}
