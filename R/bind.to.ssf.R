#' @title Function to append repro status and categorical activity rates back to extracted ssf data
#' @description Merge ssf data with original data
#' @param ssfdata data.frame of ssf data (used and available)
#' @param orig.data original data.frame with categories of movement rates
#' @return Returns a data.frame with all deer gps data needed for ssf analysis
#' @keywords deer, gps, activity
#' @export

bind.to.ssf<-function(ssfdata, orig.data){
  orig.data$IDs<-paste(orig.data$AID, orig.data$Hour, sep = "_")
  orig.uni<-unique(orig.data$IDs)

  ssfdata$Hour<-as.numeric(strftime(ssfdata$timestamp, format = "%H"))
  ssfdata$IDs<-paste(ssfdata$id, ssfdata$Hour, sep = "_")
  d<-data.frame()
  for(k in 1:length(orig.uni)){
    ssf.sub<-ssfdata[ssfdata$IDs == orig.uni[k],]
    orig.sub<-orig.data[orig.data$IDs == orig.uni[k],]

    if(nrow(ssf.sub) == 0){next}

    ssf.sub$elk.act<-orig.sub$elk.act[1]
    ssf.sub$lion.act<-orig.sub$lion.act[1]
    ssf.sub$coyote.act<-orig.sub$coyote.act[1]

    print(k)
    d<-rbind(ssf.sub, d)
  }

  new.dat<-d

  orig.data$IDs<-paste(orig.data$AID, orig.data$Date, sep = "_")
  orig.uni<-unique(orig.data$IDs)

  new.dat$date<-strftime(new.dat$timestamp, format = "%Y-%m-%d")
  new.dat$IDs<-paste(new.dat$id, new.dat$date, sep = "_")

  x<-data.frame()
  for(l in 1:length(orig.uni)){

    dsub<-new.dat[new.dat$IDs == orig.uni[l],]
    orig.sub<-orig.data[orig.data$IDs == orig.uni[l],]

    if(nrow(dsub)==0){next}

    dsub$FetusNumber<-orig.sub$FetusNumber[1]
    dsub$PregStat<-orig.sub$PregStat[1]
    dsub$AllCatch<-orig.sub$AllCatch[1]
    dsub$NeoCatch<-orig.sub$NeoCatch[1]
    dsub$ReproStatus<-orig.sub$ReproStatus[1]
    dsub$Keep<-orig.sub$Keep[1]
    dsub$birth.date<-orig.sub$birth.date[1]
    dsub$days.to.part<-orig.sub$days.to.part[1]

    print(l)
    x<-rbind(dsub, x)

  }

    return(x)
  }


