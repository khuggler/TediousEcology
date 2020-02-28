#' @title Build kernel density home ranges of individual animals
#' @description Build kernel home ranges for individual animals and produce vertices 
#' @param gps SpatialPointsDataFrame that has an 'id' column, and 'x' and 'y' column only. 'id' column should be named according to how home ranges should be built
#' @param reference bandwidth type can be either "href" or "LSCV" 
#' @param bandmethod method for choosing correct bandwidth options are "none" to stick with reference of LSCV bandwidth, or "adhoc" to use an ad-hoc smooth parameter. If ad hoc is chosen, reference bandwidth is reduced by 10% increments and all home ranges are stored in the directory
#' @param percent percent volume to be calculated and size of home range to be saved 
#' @param unout units for home range to be read out (see getverticeshr for options)
#' @param dirout path to folder where home ranges should be saved
#' @param season name of column where season (Fall/Spring) is stored
#' @return Returns a original dataframe with appended columns of IFBFat and Scaled IFBFat
#' @keywords IFBFat, body condition, max thicknes, weight
#' @export
#' 

indiv.hr<-function(gps){
new.list<-list()
  uni<-unique(gps$id)
  
  for(k in 1:length(uni)){
    sub.gps<-gps[gps$id == uni[k],]
    kern.ref<-adehabitatHR::kernelUD(sub.gps, h = reference, grid = xy)
    h<-as.numeric(kern.ref[[1]]@h[1]) 
    

  if(bandmethod == "adhoc"){
    props<-c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
    
    for(i in 1:length(props)){
      kerns<-adehabitatHR::kernelUD(sub.gps, h = props[i]*h, grid = xy)
      #image(kern.ref)
      
      hr.poly<-adehabitatHR::getverticeshr(kerns, percent = percent, unout = unout)
      plot(hr.poly)
      
      writeOGR(hr.poly, paste0(dirout, props[i],uni[k],'HR.shp'), layer = "hr", driver = 'ESRI Shapefile')
     
      print(paste(k,i, sep = "_")) 
    }
  }
    
    if(bandmethod == "none"){
     new.list[[k]]<-kern.ref[[k]]
     return(new.list)
    }
    
  }
  
}