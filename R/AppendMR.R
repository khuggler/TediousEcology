#' @title Function to append movement rate categories to deer data
#' @description Merge cleaned GPS data with the movement rates of elk, lions, and coyotes
#' @param deerdata data.frame of deer gps data with repro info
#' @param elkdata data.frame of elk gps data with MRs
#' @param liondata data.frame of lion gps data with MRs
#' @param coyotedata data.frame of coyote gps data with MRs
#' @return Returns a data.frame with all deer gps data with repro status and activity of lions, elk, and coyotes
#' @keywords deer, gps, activity
#' @export
AppendMR<-function(deerdata,elkdata,liondata, coyotedata){
  deerdata$Hour<-strftime(deerdata$TelemDate, format = "%H", tz = "MST")
  deerdata$Hour<-as.numeric(deerdata$Hour)
  
  deerdata<-deerdata[complete.cases(deerdata$Hour),]
  elkdata<-elkdata[complete.cases(elkdata$Hour),]
  liondata<-liondata[complete.cases(liondata$Hour),]
  coyotedata<-coyotedata[complete.cases(coyotedata$Hour),]
  
  uni<-unique(deerdata$Hour)
  uni<-as.numeric(uni)
  x<-data.frame()
  for(i in 1:length(uni)){
    deersub<-deerdata[deerdata$Hour == uni[i],]
    elksub<-elkdata[elkdata$Hour == uni[i],]$act.cat[1]
    lionsub<-liondata[liondata$Hour == uni[i],]$act.cat[1]
    coyotesub<-coyotedata[coyotedata$Hour == uni[i],]$act.cat[1]

    deersub$elk.act<-elksub
    deersub$lion.act<-lionsub
    deersub$coyote.act<-coyotesub

    x<-rbind(deersub, x)

  }


  return(x)
}
