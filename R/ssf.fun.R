#' @title build ssf with deer data
#' @description  sample available points from distribution for ssf data
#' @param dat deer data
#' @param datecol name of column where telemdate is stored
#' @param idcol name of column where aid is stored
#' @param plot TRUE/FALSE whether plots should be generated
#' @param pathout path to where ssf data should be written
#' @param nsamps number of random points for every step
#' @return Returns a data.frame object with all sampled ssf data
#' @keywords ssf, step selection, sample, distribution
#' @export

ssf.fun<-function(dat, datecol,idcol, plot, pathout, nsamps){
  library(ggplot2)
  library(RStoolbox)
  library(survival)
  library(MASS)
  library(lubridate)
  library(dplyr)
  library(nlme)
  library(pbs)
  library(circular)
  library(CircStats)
  #library(ssf)
  library(amt)
  library(move)
  library(tibble)
  library(leaflet)
  library(dplyr)
  library(ggmap)
  library(purrr)

  dat<-dat[dat$HDOP < 12,] ### remove shitty points


  deer7<-dat %>% filter(AIDYr == "105_2017")
  #z<-(calc_zoom(Longitude, Latitude, deer7))

  ### Create tracks in AMT package ###
  spatial.dat<-dat
  coordinates(spatial.dat)<-c('Longitude', 'Latitude')
  proj<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  proj4string(spatial.dat)<-proj

  trk <- mk_track(dat, .x=Longitude, .y=Latitude, .t=TelemDate, id = AIDYr,
                  crs = CRS(proj))

  geo.proj<-'+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

  trk<-transform_coords(trk, CRS(geo.proj))
  trk.class<-class(trk)

  #### Calculate move stats for all IDs ###

  nest.track<-trk%>%nest(-id)
  nest.track

  trk<-trk %>% nest(-id) %>%
    mutate(dir_abs = map(data, direction_abs,full_circle=TRUE, zero="N"),
           dir_rel = map(data, direction_rel),
           sl = map(data, step_lengths),
           nsd_=map(data, nsd))%>%unnest()

  trk<-trk%>%
    mutate(
      week=week(t_),
      month = month(t_),
      year=year(t_),
      hour = hour(t_)
    )

  class(trk)<-trk.class

  #### Plot some shit ####

  if(plot == TRUE){

    api<-'AIzaSyBDm3ST6ENqWojHznMUb2-PY2-5m6mgRt4'
    ggmap::register_google(api)
  map <- get_map(location = c(lon = mean(deer7$Longitude),
                              lat = mean(deer7$Latitude)), zoom = 12,
                 maptype = "hybrid", source = "google")
  ggmap(map) +
    geom_point(data=deer7, aes(x=Longitude, y=Latitude), size=2.5)

  ## Use leaflet ##
  leaflet(deer7)%>%addTiles()%>%
    addCircles(deer7$Longitude, deer7$Latitude)

  ### Plot all ids using ggmap ###
  ggplot(dat, aes(x=Longitude, y=Latitude))+geom_point()+
    facet_wrap(~AIDYr, scales="free")

  ### Now all on one map ###

  ggplot(dat, aes(x=Longitude, y=Latitude, color=as.factor(AIDYr)))+
    geom_point()


  ggplot(trk, aes(x = dir_abs, y=..density..)) + geom_histogram(breaks = seq(0,360, by=20))+
    coord_polar(start = 0) + theme_minimal() +
    scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") +
    scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 360, by=20),
                       labels = seq(0, 360, by=20))+
    facet_wrap(~id)


  ggplot(trk, aes(x = dir_rel, y=..density..)) + geom_histogram(breaks = seq(-180,180, by=20))+
    coord_polar(start = 0) + theme_minimal() +
    scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") +
    scale_x_continuous("", limits = c(-180, 180), breaks = seq(-180, 180, by=20),
                       labels = seq(-180, 180, by=20))+
    facet_wrap(~id)

  ggplot(trk, aes(x = dir_rel)) +  geom_histogram(breaks = seq(-180,180, by=20))+
    theme_minimal() +
    scale_fill_brewer() + ylab("Count") + ggtitle("Angles Relative") +
    scale_x_continuous("", limits = c(-180, 180), breaks = seq(-180, 180, by=20),
                       labels = seq(-180, 180, by=20))+facet_wrap(~id, scales="free")


  ggplot(trk, aes(x = t_, y=nsd_)) + geom_point()+
    facet_wrap(~id, scales="free")

  #### mcps ####
  mcps.week<-trk %>% nest(-id,-year,  -month, -week) %>%
    mutate(mcparea = map(data, ~hr_mcp(., levels = c(0.95)) %>% hr_area)) %>%
    select(id, year, month, week, mcparea) %>% unnest()

  ggplot(mcps.week, aes(x = week, y = area, colour=as.factor(year))) + geom_point()+
    geom_smooth()+ facet_wrap(~id, scales="free")

  ##### kdes ######

  kde.week<-trk %>% nest(-id,-year,  -month, -week) %>%
    mutate(kdearea = map(data, ~hr_kde(., levels=c(0.95)) %>% hr_area)) %>%
    select(id, year, month, week,  kdearea) %>% unnest()

  ggplot(kde.week, aes(x = week, y = kdearea, colour=as.factor(year))) + geom_point()+
    geom_smooth()+ facet_wrap(~id, scales="free")
  }

  ##### Prepping SSF data #####

  (timestats<-trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
     dplyr::select(id, sr) %>% unnest)

  trk<-trk %>% group_by(id) %>% mutate(dt_ = t_ - lag(t_, default = NA))


  #### Re sample tracks and append bursts to each id #####

 trk %>% nest(-id) %>% mutate(sr = map(.$data, summarize_sampling_rate)) %>%
    dplyr::select(id, sr) %>% unnest()

  ssfdat<- trk %>% nest(-id) %>%
    mutate(ssf = map(data, function(d){
      d %>%
        track_resample(rate = hours(1), tolerance = minutes(15)) %>%
        filter_min_n_burst(min_n = 3) %>%
        steps_by_burst() %>% random_steps(nsamps) ## can specify number of random steps desired
    })) %>% dplyr::select(id, ssf) %>% unnest()


  ssfdat$utm.easting<-ssfdat$x2_
  ssfdat$utm.northing<-ssfdat$y2_

  ssfdat2 <- SpatialPointsDataFrame(ssfdat[,c("x2_","y2_")], ssfdat,
                                    proj4string=CRS("+proj=utm +zone=12N +datum=WGS84"))
  ssf.df <- data.frame(spTransform(ssfdat2, CRS("+proj=longlat +datum=WGS84")))
  names(ssf.df)[c(1,16,17)] <-c("AIDYr", "long", "lat")
  ssf.df$timestamp<-ssf.df$t1_
  ssf.df<-ssf.df[,c('AIDYr', 'timestamp', 'burst_', 'step_id_','case_', 'lat','long', 'utm.easting', 'utm.northing', 'x1_', 'x2_', 'y1_', 'y2_', 'sl_', 'ta_')]

  write.csv(ssf.df,pathout, row.names = F)

 return(ssf.df)

}
