#' @title append age to gps data
#' @description append ages of animals to gps data
#' @param cappath path to where capture data (with birth dates) lives
#' @param idcol name of column where animal ID exists
#' @param gps name of gps dataframe
#' @param type type of append to do. Either "gps" or "database"
#' @return Returns original gps data with an age column
#' @keywords age, capture, gps
#' @export

append.age<-function(agepath, cappath, idcol, gps, type){


  if(type == "gps"){
  sub<-read.csv(cappath, stringsAsFactors = F)
  sub$BirthDate<-as.Date(sub$BirthDate, format = "%m/%d/%Y")
  sub.gps<-gps
  sub.gps$Date<-strftime(sub.gps$TelemDate, format = "%Y-%m-%d")

  uni<-unique(sub[,idcol])
  all.dat<-data.frame()
  for(i in 1:length(uni)){
    subsub<-sub[sub[,idcol] == uni[i],]
    new.gps<-sub.gps[sub.gps$AID == uni[i],]

    new.gps$diff<-as.numeric(difftime(new.gps$Date, subsub$BirthDate[1], units = "days")/365)
    new.gps$age<-round(new.gps$diff/0.5)*0.5

    all.dat<-rbind(new.gps, all.dat)

      }

  }

  if(type == "database"){
    sub<-read.csv(cappath, stringsAsFactors = F)
    sub$BirthDate<-as.Date(sub$BirthDate, format = "%m/%d/%Y")
    sub$CaptureDate<-as.Date(sub$CaptureDate, format = "%m/%d/%Y")
    uni<-unique(sub[,idcol])

    all.dat<-data.frame()
    for(k in 1:length(uni,)){
      subsub<-sub[sub$UAID == uni[k],]
      subsub$diff<-as.numeric(difftime(subsub$CaptureDate, subsub$BirthDate[1], units = "days")/365)
      subsub$Age<-round(subsub$diff/0.5)*0.5


      all.dat<-rbind(all.dat, subsub)
    }
  }


  return(all.dat)
}

