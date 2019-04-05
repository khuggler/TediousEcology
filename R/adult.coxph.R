#' @title Stack and extract all spatial data to elk, coyotes, and mountain lions (RF Encounter)
#' @description Stack rasters and extract points data for elk, coyotes, and mountain lions
#' @param data to be used to build RF model
#' @param uni number of cpus to parallelize on
#' @param uaidcol percent of data (decimal format) to withold for training model (default 0.25)
#' @param startcol number of trees (maximum) to attempt to split on
#' @param mortcol path to raster stack
#' @param yearstart path to study area polygon (where you want to define availability)
#' @param yearend path to where predicted RF map should be written
#' @param seasons duh
#' @param winterstart duh
#' @param winterend duh
#' @param cause duh
#' @param plot duh
#' @param title duh
#' @return Returns a list object with RFData necessary to predict probably of use in RF models (elk, coyotes, and mountain lions)
#' @keywords elk, coyote, mountain lion, random forest, extract, raster, sample
#' @export


adult.cox.mods<-function(data, uni, uaidcol,startcol,mortcol, yearstart, yearend, seasons, winterstart, winterend, cause, plot, title){

  survdat<-TediousEcology::AdultSeasonalSurv(data = data, uni = uni, UAIDcol = UAIDcol, startcol = startcol, mortcol = mortcol, yearstart = yearstart,
                                             yearend = yearend, seasons = seasons, winterstart = winterstart, winterend = winterend,
                                             cause = cause, plot = plot, title = title)


}
