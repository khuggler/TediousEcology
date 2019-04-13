#' @title Conversion of adult capture data into capture history data
#
#' @description Convert capture data into a record of every serial number and start and end dates of every individual captured
#' @param capdat data.frame with all the capture information
#' @param df format of CaptureDate and MortalityDate
#' @return Returns a data.frame with start and end date of every individual serial number along with an assigned animal ID)
#' @keywords capture, animal ID, history
#' @export
#' @examples
#' \donttest{Hist<-CollarHistory(capdat = yourdata, format = %m/%d/%Y)}

CollarHistory<-function(capdat, df){

  dat<-read.csv(capdat,stringsAsFactors = F)
  dat$CaptureDate<-as.Date(dat$CaptureDate,tryFormats = c('%m/%d/%Y', '%Y-%m-%d'))
  dat$MortalityDate<-as.Date(dat$MortalityDate,tryFormats = c('%m/%d/%Y', '%Y-%m-%d'))

  dat$Uni<-paste(dat$New.Serial.Number,dat$CaptureDate,dat$Mortality.,sep='_')

  dat$NewCap<-ifelse(is.na(dat$Old.Serial.Number),1,0)

  ad<-dat[,c(4,27,28,46:49)]

  #adnew<-ad[ad$NewCap==1,]
  #adold<-ad[ad$NewCap==0,]
  uaid<-data.frame()

  unik<-unique(dat$Uni[dat$NewCap==1])


  for(i in 1:length(unik)){
    sub<-dat[dat$Uni==unik[i],]
    sub<-sub[complete.cases(sub$CaptureDate),]
    sub<-sub[!duplicated(sub),]

    sub2<-dat[dat$Old.Serial.Number==sub$New.Serial.Number[1]&dat$CaptureDate>sub$CaptureDate[1],]

    sub2<-sub2[complete.cases(sub2$CaptureDate),]
    sub2<-sub2[1,]
    sub2$New.Serial.Number<-ifelse(is.na(sub2$New.Serial.Number)&sub2$Mortality.=='0',sub2$Old.Serial.Number,sub2$New.Serial.Number)

    sub3<-dat[dat$Old.Serial.Number==sub2$New.Serial.Number[1]&dat$CaptureDate>sub2$CaptureDate[1],]

    sub3<-sub3[complete.cases(sub3$CaptureDate),]
    sub3<-sub3[1,]
    sub3$New.Serial.Number<-ifelse(is.na(sub3$New.Serial.Number)&sub3$Mortality.=='0',sub3$Old.Serial.Number,sub3$New.Serial.Number)


    sub4<-dat[dat$Old.Serial.Number==sub3$New.Serial.Number[1]&dat$CaptureDate>sub3$CaptureDate[1],]

    sub4<-sub4[complete.cases(sub4$CaptureDate),]
    sub4<-sub4[1,]
    sub4$New.Serial.Number<-ifelse(is.na(sub4$New.Serial.Number)&sub4$Mortality.=='0',sub4$Old.Serial.Number,sub4$New.Serial.Number)


    sub5<-dat[dat$Old.Serial.Number==sub4$New.Serial.Number[1]&dat$CaptureDate>sub4$CaptureDate[1],]

    sub5<-sub5[complete.cases(sub5$CaptureDate),]
    sub5<-sub5[1,]
    sub5$New.Serial.Number<-ifelse(is.na(sub5$New.Serial.Number)&sub5$Mortality.=='0',sub5$Old.Serial.Number,sub5$New.Serial.Number)


    sub6<-dat[dat$Old.Serial.Number==sub5$New.Serial.Number[1]&dat$CaptureDate>sub5$CaptureDate[1],]

    sub6<-sub6[complete.cases(sub6$CaptureDate),]
    sub6<-sub6[1,]
    sub6$New.Serial.Number<-ifelse(is.na(sub6$New.Serial.Number)&sub6$Mortality.=='0',sub6$Old.Serial.Number,sub6$New.Serial.Number)


    sub7<-dat[dat$Old.Serial.Number==sub6$New.Serial.Number[1]&dat$CaptureDate>sub6$CaptureDate[1],]

    sub7<-sub7[complete.cases(sub7$CaptureDate),]
    sub7<-sub7[1,]
    sub7$New.Serial.Number<-ifelse(is.na(sub7$New.Serial.Number)&sub7$Mortality.=='0',sub7$Old.Serial.Number,sub7$New.Serial.Number)


    sub<-rbind(sub,sub2,sub3,sub4,sub5,sub6,sub7)
    sub<-sub[complete.cases(sub$CaptureDate),]

    if('1' %in% sub$Mortality.){
      mdate<-min(sub$MortalityDate,na.rm=T)
      sub<-sub[sub$CaptureDate<=mdate,]
    }

    uni<-unique(sub$Old.Serial.Number,sub$New.Serial.Number)
    uni<-uni[!is.na(uni)]


    sub$StartDate<-NA
    sub$StartDate[1]<-as.character(sub$CaptureDate[1])
    sub$EndDate<-NA
    sub$New.Serial.Number<-ifelse(is.na(sub$New.Serial.Number),sub$Old.Serial.Number,sub$New.Serial.Number)

    if(nrow(sub)>1){
      alk<-sub[1,]
      for(k in 2:nrow(sub)){
        subsub<-sub[k,]
        if(subsub$Old.Serial.Number==alk$New.Serial.Number[nrow(alk)]){
          alk<-rbind(alk,subsub)
        }
      }
    }else{
      alk<-sub
    }


    sub<-alk
    if(nrow(sub)>1){
      for(l in 2:nrow(sub)){

        sub$StartDate[l]<-ifelse(sub$Old.Serial.Number[l]==sub$New.Serial.Number[l],NA,as.character(sub$CaptureDate[l]))

        sub$EndDate[l-1]<-ifelse(!(sub$Old.Serial.Number[l]==sub$New.Serial.Number[l]),as.character(sub$CaptureDate[l]),NA)
        sub$EndDate[l]<-ifelse(sub$Mortality.[l]==1,as.character(sub$MortalityDate[l]),sub$EndDate[l])
        if(i == nrow(sub) & is.na(sub$EndDate[l])){
          sub$EndDate[l]<-as.character(Sys.Date())
        }
      }
    }
    if(nrow(sub)==1 & '1' %in% sub$Mortality.){
      sub$EndDate<-as.character(sub$MortalityDate)
    }
    if(nrow(sub)==1 & '0' %in% sub$Mortality.){
      sub$EndDate<-as.character(Sys.Date())
    }
    if(is.na(sub$EndDate[nrow(sub)])){
      sub$EndDate[nrow(sub)]<-as.character(Sys.Date())
    }
    sub$StartDate<-as.Date(sub$StartDate,format='%Y-%m-%d')
    sub$EndDate<-as.Date(sub$EndDate,format='%Y-%m-%d')

    agg<-aggregate(sub$StartDate,by=list(sub$New.Serial.Number),FUN=min,na.rm=T)
    agg[,3]<-aggregate(sub$EndDate,by=list(sub$New.Serial.Number),FUN=max,na.rm=T)[,2]

    names(agg)<-c('Serial','StartDate','EndDate')

    iin<-data.frame(UAID=i,Ser1=NA,Ser1Start=as.Date(NA),Ser1End=as.Date(NA),
                    Ser2=NA,Ser2Start=as.Date(NA),Ser2End=as.Date(NA),
                    Ser3=NA,Ser3Start=as.Date(NA),Ser3End=as.Date(NA),
                    Ser4=NA,Ser4Start=as.Date(NA),Ser4End=as.Date(NA),
                    stringsAsFactors = F)
    agg<-agg[order(agg$StartDate),]
    #for(i in 1:nrow(sub)){
    iin[1,2]<-agg$Serial[1]
    iin[1,3]<-agg$StartDate[1]
    iin[1,4]<-agg$EndDate[1]

    iin[1,5]<-agg$Serial[2]
    iin[1,6]<-agg$StartDate[2]
    iin[1,7]<-agg$EndDate[2]

    iin[1,8]<-agg$Serial[3]
    iin[1,9]<-agg$StartDate[3]
    iin[1,10]<-agg$EndDate[3]

    iin[1,11]<-agg$Serial[4]
    iin[1,12]<-agg$StartDate[4]
    iin[1,13]<-agg$EndDate[4]


    uaid<-rbind(uaid,iin)
  }
  return(uaid)

}
