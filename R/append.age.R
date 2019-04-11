#' @title append ages and calculate new age for every capture event
#' @description append ages of animals to databaes and calculate age for every capture event
#' @param agepath path to where age table lives
#' @param cappath path to where capture data lives
#' @param pathout path to where new data.frame should be written
#' @return Returns original capture data.frame with age column of each animal
#' @keywords age, capture
#' @export

append.age<-function(agepath, cappath, pathout){
### append AID to ages ###
age<-read.csv(agepath, stringsAsFactors = F)
age<-age[,c(1:11)]
age$uni<-paste(age$Species, age$Tooth.ID, age$Sex,sep = "_")
uni<-unique(age$uni)
age<-age[!duplicated(age),]

cap<-read.csv(cappath, stringsAsFactors = F)
cap$Species<-ifelse(cap$Species == "MMD", "MD", cap$Species)
cap$Sex<-ifelse(cap$Sex == "f" | cap$Sex == "F", "F", "M")
cap$uni<-paste(cap$Species, cap$Lab.Accession, cap$Sex, sep = "_")

new.data<-data.frame()
for(k in 1:length(uni)){
  agesub<-age[age$uni == uni[k],]
  agesub$Date<-as.Date(agesub$Date,tryFormats = c("%m/%d/%Y", '%Y-%m-%d'))
  cap$CaptureDate<-as.Date(cap$CaptureDate,tryFormats = c("%m/%d/%Y", '%Y-%m-%d'))

  aid<-cap[cap$uni == uni[k],]$UAID
  capsub<-cap[cap$UAID %in% aid,]
  capsub$age<-agesub$Age
  startdate<-capsub[1,]$CaptureDate
  startdate<-as.Date(startdate, tryFormats = c("%m/%d/%Y", '%Y-%m-%d'))

  sub<-capsub
  sub$diff<-abs(as.numeric(difftime(startdate, sub$CaptureDate, units = "days"))/365)
  sub$new.age<-sub$age + sub$diff

  new.data<-rbind(new.data, sub)
  new.dat<-new.data[,c(1:58, 62)]

  write.csv(new.dat, pathout, row.names = F)
}
return(new.dat)
}
