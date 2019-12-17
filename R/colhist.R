#' @title Conversion of adult capture data into capture history data
#' @description Convert capture data into a record of every serial number and start and end dates of every individual captured
#' @param capdat data.frame with all the capture information
#' @return Returns a data.frame with start and end date of every individual serial number along with an assigned animal ID)
#' @keywords capture, animal ID, history
#' @export
#' @examples
#' \donttest{Hist<-CollarHistory(capdat = yourdata)}

colhist<-function(capdat){

cap<-read.csv(capdat, stringsAsFactors = F)
cap$CaptureDate<-as.Date(cap$CaptureDate, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))
cap$MortalityDate<-as.Date(cap$MortalityDate, format = '%m/%d/%Y')

uni<-unique(cap$UAID)

colhist<-data.frame(AID = NA, Ser1 = NA, Ser1Start = NA, Ser1End = NA,
                  Ser2 = NA, Ser2Start = NA, Ser2End = NA,
                    Ser3 = NA, Ser3Start = NA, Ser3End = NA,
                    Ser4 = NA, Ser4Start = NA, Ser4End = NA,
                   Ser5 = NA, Ser5Start = NA, Ser5End = NA)

for(k in 1:length(uni)){
  sub<-cap[cap$UAID == uni[k],]



 sub$Serial<-ifelse(is.na(sub$Old.Serial.Number) & !is.na(sub$New.Serial.Number), sub$New.Serial.Number, ifelse(!is.na(sub$Old.Serial.Number) & !is.na(sub$New.Serial.Number), sub$New.Serial.Number,
                                                                                                                sub$Old.Serial.Number))
x<-nrow(sub)


sub[x,]$MortalityDate<-ifelse(!is.na(sub[x,]$MortalityDate), as.character(sub[x,]$MortalityDate), as.character(Sys.Date()))
sub[x,]$MortalityDate<-as.Date(sub[x,]$MortalityDate, format = "%Y-%m-%d")

unisers<-unique(sub$Serial)


 colhist[k,]<-NA

 for(i in 1:length(unisers)){


   colsub<-sub[sub$Serial == unisers[i],]
   


   if(i ==1){
     colhist$Ser1[k]<-colsub$Serial[1]
     colhist$Ser1Start[k]<-as.character(colsub$CaptureDate[1])
     x<-nrow(colsub)
     colhist$Ser1End[k]<-ifelse(!is.na(colsub$MortalityDate[x]), as.character(colsub$MortalityDate[x]), NA)
   }

   if(i == 2){
     colhist$Ser2[k]<-colsub$Serial[1]
     colhist$Ser2Start[k]<-as.character(colsub$CaptureDate[1])
     colhist$Ser1End[k]<-as.character(colsub$CaptureDate[1]-1)
     x<-nrow(colsub)
     colhist$Ser2End[k]<-ifelse(!is.na(colsub$MortalityDate[x]), as.character(colsub$MortalityDate[x]), NA)

   }
   


   if(i == 3){
     colhist$Ser3[k]<-colsub$Serial[1]
     colhist$Ser3Start[k]<-as.character(colsub$CaptureDate[1])
     colhist$Ser2End[k]<-as.character(colsub$CaptureDate[1]-1)
     x<-nrow(colsub)
     colhist$Ser3End[k]<-ifelse(!is.na(colsub$MortalityDate[x]), as.character(colsub$MortalityDate[x]), NA)

   }

   if(i == 4){
     colhist$Ser4[k]<-colsub$Serial[1]
     colhist$Ser4Start[k]<-as.character(colsub$CaptureDate[1])
     colhist$Ser3End[k]<-as.character(colsub$CaptureDate[1]-1)
     x<-nrow(colsub)
     colhist$Ser4End[k]<-ifelse(!is.na(colsub$MortalityDate[x]), as.character(colsub$MortalityDate[x]), NA)

   }

   if(i == 5){
     colhist$Ser5[k]<-colsub$Serial[1]
     colhist$Ser5Start[k]<-as.character(colsub$CaptureDate[1])
     colhist$Ser4End[k]<-as.character(colsub$CaptureDate[1]-1)
     x<-nrow(colsub)
     colhist$Ser5End[k]<-ifelse(!is.na(colsub$MortalityDate[x]), as.character(colsub$MortalityDate[x]), NA)

   }

   colhist$AID[k]<-colsub$UAID[1]


 }
}

colhist$Ser1Start<-as.Date(colhist$Ser1Start, tryFormats= c('%m/%d/%Y', '%Y-%m-%d'))
colhist$Ser1End<-as.Date(colhist$Ser1End,  tryFormats= c('%m/%d/%Y', '%Y-%m-%d'))

colhist$Ser2Start<-as.Date(colhist$Ser2Start,  tryFormats= c('%m/%d/%Y', '%Y-%m-%d'))
colhist$Ser2End<-as.Date(colhist$Ser2End,  tryFormats= c('%m/%d/%Y', '%Y-%m-%d'))

colhist$Ser3Start<-as.Date(colhist$Ser3Start,  tryFormats= c('%m/%d/%Y', '%Y-%m-%d'))
colhist$Ser3End<-as.Date(colhist$Ser3End,  tryFormats= c('%m/%d/%Y', '%Y-%m-%d'))

colhist$Ser4Start<-as.Date(colhist$Ser4Start,  tryFormats= c('%m/%d/%Y', '%Y-%m-%d'))
colhist$Ser4End<-as.Date(colhist$Ser4End,  tryFormats= c('%m/%d/%Y', '%Y-%m-%d'))

colhist$Ser5Start<-as.Date(colhist$Ser5Start,  tryFormats= c('%m/%d/%Y', '%Y-%m-%d'))
colhist$Ser5End<-as.Date(colhist$Ser5End,  tryFormats= c('%m/%d/%Y', '%Y-%m-%d'))

colhist$Ser3<-ifelse(colhist$Ser3 == 37102, 37101, colhist$Ser3)

return(colhist)
  }

