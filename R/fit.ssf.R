### Fit SSF Model ###
all.data<-data.frame(all.data)
all.data$RasterStack_NLCD_11_30<-as.factor(all.data$RasterStack_NLCD_11_30)


space.only <- clogit(case_ ~ elkhigh + elklow + lionhigh + lionlow + yotehigh + yotelow + PredRiskFinal:lion.act + strata(step_id_) + cluster(id), method = "efron", data = all.data)
summary(space.only)

all.data$coyote.act<-as.factor(all.data$coyote.act)
all.data$lion.act<-as.factor(all.data$lion.act)
all.data$elk.act<-as.factor(all.data$elk.act)
space.time<- clogit(case_ ~ elkhigh*elk.act + elklow*elk.act +
                      yotehigh*coyote.act + yotelow*coyote.act +
                      lionhigh*lion.act + lionlow*lion.act +
                      PredRiskFinal*lion.act + strata(step_id_) + cluster(id), method = "efron", data = all.data)
summary(space.time)
space.time2<-clogit(case_ ~ ElkRFMap:elk.act + YoteRFMap:coyote.act + LionRFMap:lion.act + PredRiskFinal:lion.act + PredRiskFinal + ElkRFMap + YoteRFMap + LionRFMap + strata(step_id_) + cluster(id),method = "efron", data = all.data)
