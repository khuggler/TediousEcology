#' @title append age to gps data 
#' @description append ages of animals to gps data 
#' @param agepath path to age table
#' @param dbpath path to capture database
#' @return Returns original gps data with an age column 
#' @keywords age, capture, gps 
#' @export


birth.dates<-function(agepath, dbpath){
age<-read.csv('C:/Users/khuggler/Box Sync/DEER/Data/CleanDatabase/CleanAgeTable.csv', stringsAsFactors = F)
db<-read.csv('C:/Users/khuggler/Box Sync/DEER/Data/CleanDatabase/MasterDB.csv', stringsAsFactors = F)

uni<-unique(db$UAID)
all.db<-data.frame()
for(i in 1:length(uni)){
  agesub<-age[age$AID == uni[i],]
  dbsub<-db[db$UAID == uni[i],]
  
  if(nrow(agesub) == 0 & nrow(dbsub)>= 1){
    dbsub$BirthDate<-NA
  }
  
  if(nrow(dbsub)>= 1 & nrow(agesub) == 1){
    dbsub$BirthDate<-NA
    dbsub[1, 'BirthDate']<-agesub$BirthDate
  }
  
  if(nrow(dbsub) == 0 & nrow(agesub) ==1){
    dbsub$BirthDate<-NA
  }
  
  all.db<-rbind(dbsub, all.db)
}

write.csv(all.db, dbpath, row.names = F)

return(all.db)
}