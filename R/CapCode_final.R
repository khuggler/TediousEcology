### take a look at AIDs that should be captured, based on everything that was captured as of April 2018 ###
cap<-read.csv('C:/Users/khuggler/Box Sync/DEER/Data/CleanDatabase/DB_WithAID_03102019.csv', stringsAsFactors = F)
cap$CaptureDate<-as.Date(cap$CaptureDate, tryFormats = c('%Y-%m-%d', '%m/%d/%Y'))

cap<-cap[cap$CaptureDate > "2018-04-01",]
cap<-cap[cap$Sex == "F",]
cap$Species<-ifelse(cap$Species == "ELK", "E", cap$Species)
#cap<-cap[cap$Mortality. !=1,]

uni<-unique(cap$UAID)
p<-data.frame()
for(k in 1:length(uni)){
  sub<-cap[cap$UAID == uni[k],]
  if(nrow(sub)>1){
    x<-nrow(sub)
    sub<-sub[x,]
  }
  p<-rbind(sub, p)
}

morts<-p[p$Mortality. == 1,]$UAID

p<-p[!p$UAID %in% morts,] ## check 31812

### This looks correct, now make UID column ###
p$Freq<-ifelse(!is.na(p$Old.Frequency) & !is.na(p$New.Frequency), p$New.Frequency, ifelse(is.na(p$Old.Frequency) & !is.na(p$New.Frequency), p$New.Frequency,
                                                                                          p$Old.Frequency))
p$unique<-paste(p$Freq, p$Species, sep = "")

p<-p[, c('unique', 'UAID')]

### Merge remaining fawn information into this table
fawn<-read.csv('C:/Users/khuggler/Box Sync/DEER/Data/CleanDatabase/Neonate_MasterDB_03222019.csv', stringsAsFactors = F)
fawn$StartDate<-as.Date(fawn$StartDate, tryFormats = c('%Y-%m-%d', '%m/%d/%Y'))
fawn$Year<-strftime(fawn$StartDate, format = "%Y")
fawn<-fawn[fawn$Year == "2018" & fawn$Spp == "MD",]
fawn<-fawn[fawn$EndDate == "",]
fawn$uni<-paste(fawn$FawnFreq, "FA", sep = "")

fawnsers<-fawn$FawnID

#### Loop through and append fawn information to mom info ####

uni<-unique(p$UAID)
p$UAID<-as.character(p$UAID)
all<-data.frame()
for(x in 1:length(uni)){
  sub<-p[p$UAID == uni[x],]
  fawnsub<-fawn[fawn$MomAID == uni[x],]
  if(nrow(fawnsub) == 0){
    sub$FawnAlive<-0
    sub$FawnID<-NA
  }
  if(nrow(fawnsub)>0){
  sub$FawnAlive<-1
  sub$FawnID<-"1F"
  }

  all<-rbind(sub, all)
}
all$unique<-ifelse(is.na(all$FawnID), all$unique, paste(all$unique, all$FawnID, sep = ""))

###### Download all deer and elk data ######
gps<-TediousEcology::CleanGPSData(vecdata = TRUE, vecpath = 'C:/Users/khuggler/Box Sync/SummerPart/VecData/',
                                  usernames = c("KE12885MO", "KE13307MO"), passwords = c('C2fH0F&D','P3fP7V$R'),
                                  tempdir = 'C:/Users/khuggler/Desktop/', ST = TRUE, STUser = 'srsdeerproject@gmail.com',
                                  STPass = 'wyoming1', cType = "ATS/IRID", capdat = 'C:/Users/khuggler/Box Sync/DEER/Data/CleanDatabase/DB_WithAID_03102019.csv', dateformat = '%Y-%m-%d %H:%M:%S')

adult.gps<-gps[gps$AID %in% all$UAID,]
neo.gps<-Part::getVec('C:/Users/khuggler/Box Sync/SummerPart/VecData/')
proj<-sp::proj4string(neo.gps)
neo.gps<-neo.gps[neo.gps$CollarSerialNumber %in% fawnsers, ]

############## run through gps data and merge uids ##############
uni<-unique(all$UAID)
adult.dat<-NULL
for(i in 1:length(uni)){
  sub<-adult.gps[adult.gps$AID == uni[i],]
  subsub<-all[all$UAID == uni[i],]
  sub$uid<-subsub$unique
  
  x<-nrow(sub)
  sub2<-sub[x,]
  max.date<-as.Date(sub2$TelemDate, format = "%Y-%m-%d")
  sub$FailedCollar<-ifelse(max.date < Sys.Date()-15, 1, 0)
  sub$MomAlive<-NA
  adult.dat<-rbind(sub, adult.dat)
}

###### merge ids to fawn data ########
fawn.dat<-NULL
for(i in 1:length(fawnsers)){
  sub<-data.frame(neo.gps[neo.gps$CollarSerialNumber == fawnsers[i],])
  subsub<-fawn[fawn$FawnID == fawnsers[i],]
  sub$Sex<-subsub$Sex
  sub$AID<-subsub$MomAID
  sub$uid<-subsub$uni
  
  x<-nrow(sub)
  sub2<-sub[x,]
  max.date<-as.Date(sub2$TelemDate, format = "%Y-%m-%d")
  sub$FailedCollar<-ifelse(max.date < Sys.Date()-15, 1, 0)
  sub$MomAlive<-ifelse(sub$AID %in% adult.dat$AID, 1, 0)
  fawn.dat<-rbind(sub, fawn.dat)
}

adult.dat<-adult.dat[,c(1,2,3,4,9,10,13,14,15)]
names(adult.dat)[3:4]<-c('y','x')
adult.dat$Sex<-as.character(adult.dat$Sex)
adult.dat$CollarSerialNumber<-as.character(adult.dat$CollarSerialNumber)
fawn.dat<-fawn.dat[,c(1,2,6,7,10,9,11,12, 13)]
names(fawn.dat)[3:4]<-c('y', 'x')
fawn.dat$CollarSerialNumber<-as.character(fawn.dat$CollarSerialNumber)

all.dat<-rbind(adult.dat, fawn.dat)

caps<-Part::capdat(uid = 'uid', data = all.dat, folder = 'C:/Users/khuggler/Box Sync/DEER/Data/Capture Prep/April2019/', proj = proj, spp = "catch", TD = 'TelemDate')
