#' @title Conversion of adult capture data into the correct format for modeling annual survival
#
#' @description Convert capture and cause-specific mortality data of adults into the proper format for conducting survival analysis.This function will convert raw data
#' that contains start dates and mortality dates of individual animals and transform it into the proper format required by package survival
#' to model annual survival.
#' @param data data.frame that contains a column of unique animal identifier, start date, mortality date, and cause of mortality
#' @param format format of mortcol
#' @param uaidcol name of column where unique animal identifiers are located
#' @param capcol name of column where capture date is stored
#' @param mortcol name of column that contains the date of mortality
#' @param yearstart year desired to begin modeling survival (e.g. beginning year of study)
#' @param yearend year desired to end modeling survival
#' @param cause vector of causes that require censoring (e.g. collar_failure, capturemort, etc.)
#' @param plot logical. TRUE/FALSE. If TRUE, function will generate bar plot of yearly survival
#' @param title desired title of survival plot (character)
#' @param spp species to subset survival data to
#' @return Returns a data.frame with animal ID, start date of modeling, end date of modeling, status of animal (alive = 0, dead = 1), and number of months alive during time period
#' @keywords adult, annual, survival, kaplan-meier, analysis
#' @export
#' @examples
#' \donttest{AdultSurv<-AdultAnnualSurv(data = yourdata, uni = uniquevector, mortcol = "MortalityDate", yearstart = 2015, yearend = 2019 , cause = "CaptureMort")}
#'

AdultAnnualSurv<-function(data, dateformat, uaidcol,capcol, mortcol, yearstart, yearend, cause, plot,title, spp){
  data[,mortcol]<-as.Date(data[,mortcol], format = dateformat)
  Year<-yearstart:yearend
  hist<-data.frame(Year = Year, StartDate = paste("01/01/", Year, sep = ""), EndDate = paste("12/31/", Year, sep = ""))
  hist$StartDate<-as.Date(hist$StartDate, format = "%m/%d/%Y")
  hist$EndDate<-as.Date(hist$EndDate, format = "%m/%d/%Y")

  d<-data.frame()
  z<-data.frame()

  uni<-unique(data[,uaidcol])
  
all.surv<-data.frame()
  for(k in 1:length(uni)){
    sub<-data[data[,uaidcol] == uni[k],]
    date<-ifelse(is.na(unique(sub[nrow(sub),mortcol])) %in% FALSE,
                 as.character(max(sub[,mortcol], na.rm=T)), NA)
    if(!is.na(date)){
      date<-as.Date(date, format = "%Y-%m-%d")
    }
    if(is.na(date)){
      date<-Sys.Date()
    }
    capdate<-as.Date(sub[,capcol], tryFormats = c('%Y-%m-%d', '%m/%d/%Y'), origin = sub[,capcol])
    start<-min(capdate)
    end<-date
    

      hist$start.seq<-ifelse(start >= hist$StartDate & start <= hist$EndDate, 1, 0)
      hist$end.seq<-ifelse(end >= hist$StartDate & end <= hist$EndDate, 1, 0)
      start.row<-which(hist$start.seq == 1)
      end.row<-which(hist$end.seq == 1)
      
      if(identical(end.row, integer(0)) == TRUE){
        end.row<-nrow(hist)
      }
      
      new.hist<-hist[start.row:end.row, ]
      new.hist$start<-ifelse(new.hist$start.seq == 1, as.character(start), NA)
      new.hist$end<-ifelse(new.hist$end.seq == 1, as.character(end), NA)
      
      new.hist$time.alive<-ifelse(!is.na(new.hist$start) & is.na(new.hist$end), abs(difftime(new.hist$start, new.hist$EndDate, units = "days")/30.4375), NA)
      new.hist$time.alive<-ifelse(is.na(new.hist$start) & is.na(new.hist$end), 12, new.hist$time.alive)
      new.hist$time.alive<-ifelse(is.na(new.hist$start) & !is.na(new.hist$end), abs(difftime(new.hist$StartDate, new.hist$end, units = "days")/30.4375), new.hist$time.alive)
      new.hist$time.alive<-ifelse(!is.na(new.hist$start) & !is.na(new.hist$end), abs(difftime(new.hist$start, new.hist$end, units = "days")/30.4375), new.hist$time.alive)

      c<-nrow(sub)
      new.hist$aid<-sub$UAID[1]
      new.hist$cause<-ifelse(!is.na(new.hist$end), sub$X[c], NA)
      new.hist$sex<-sub$Sex[1]
      new.hist$spp<-sub$Species[1]
      x<-nrow(new.hist)
      new.hist$status<-ifelse(is.na(new.hist$end),0, NA)
      new.hist$status<-ifelse(!is.na(new.hist$end), 1, new.hist$status)
      new.hist$status<-ifelse(new.hist$status == 1 & new.hist$cause %in% cause, 0, new.hist$status)
      new.hist$Year<-strftime(new.hist$StartDate, format = "%Y")
      
      all.surv<-rbind(new.hist, all.surv)
      
  }








      

  if(plot == TRUE){
    all.surv<-all.surv[all.surv$spp == spp,]
    surv<-survival::survfit(survival::Surv(time = all.surv$time.alive, event = all.surv$status)~ all.surv$Year)

    summ<-summary(surv)
    cols<-lapply(c(2:6, 8:11), function(x) summ[x])
    tbl<-do.call(data.frame, cols)

    unistrat<-as.character(unique(tbl$strata))

    cumsurv<-data.frame()

    for (h in 1:length(unistrat)){
      subsurv<-tbl[tbl$strata == unistrat[h],]
      row<-nrow(subsurv)
      subsurv<-subsurv[row,]

      cumsurv<-rbind(subsurv, cumsurv)

    }
    csurv<-data.frame(Year = yearend:yearstart, Surv = cumsurv$surv)
    csurv<-csurv[order(csurv$Year),]
    library(RColorBrewer)
    n<-length(unique(csurv$Year))
    cols<-brewer.pal(n = n, name = "Set1")
    barplot(csurv$Surv, col = cols, ylim = c(0,1), names.arg = csurv$Year, border = NA, main = title)
  }
  
  if(plot ==FALSE){
    next
  }
  
  
      return(list(all.surv, csurv))
    }
