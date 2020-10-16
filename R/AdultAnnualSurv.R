#' @title Conversion of adult capture data into the correct format for modeling annual survival
#' @description Convert capture and cause-specific mortality data of adults into the proper format for conducting survival analysis.This function will convert raw data
#' that contains start dates and mortality dates of individual animals and transform it into the proper format required by package survival
#' to model annual survival.
#' @param dbpath path to data.frame (csv) that contains a column of unique animal identifier, start date, mortality date, and cause of mortality (if applicable). If mortality has not occured, mortality date will be Sys.Date(), and event indicator will be 0
#' @param mortcol name of column where mortality date exists
#' @param dateformat format of mortality and capture dates
#' @param yearstart year (YYYY) to begin modelling survival. 
#' @param yearend year (YYYY) to end modelling survival 
#' @param idcol name of column where unique animal identifiers are located
#' @param capcol name of column where capture date is stored
#' @param fatecol name of column where fate/cause of mortality occurs
#' @param censors vector of causes that require censoring (e.g. collarfailure, capturemort, etc.)
#' @param plot logical. TRUE/FALSE. If TRUE, function will generate bar plot of yearly survival
#' @param title desired title of survival plot (character)
#' @return Returns a data.frame with animal ID, start date of modeling, end date of modeling, status of animal (alive = 0, dead = 1), and number of months alive during time period
#' @keywords adult, annual, survival, kaplan-meier, analysis
#' @export
#' @examples
#' \donttest{AdultSurv<-AdultAnnualSurv(dbpath = 'C:/Desktop/yourdb', mortcol = 'MortDate', dateformat = "%m/%d/%Y", yearstart = 2019, yearend = 2020 , idcol = 'AID', capcol = 'CaptureDate',fatecol = 'Cause', censors = c("CollarFailure", "CaptureMort"), plot = TRUE, title = 'Survival of Bighorn Sheep' }
#'

AdultAnnualSurv<-function(dbpath,mortcol,dateformat, yearstart, yearend, idcol, capcol, fatecol, cause, plot,title, spp, sex){
  data<-read.csv(dbpath, stringsAsFactors = F)
  
  data[,mortcol]<-as.Date(data[,mortcol], format = dateformat)
  Year<-yearstart:yearend
  
  #' create dataframe with dates 
  hist<-data.frame(Year = Year, StartDate = paste("01/01/", Year, sep = ""), EndDate = paste("12/31/", Year, sep = ""))
  hist$StartDate<-as.Date(hist$StartDate, format = "%m/%d/%Y")
  hist$EndDate<-as.Date(hist$EndDate, format = "%m/%d/%Y")

  d<-data.frame()
  z<-data.frame()

  uni<-unique(data[,idcol])
  
all.surv<-data.frame()

  for(k in 1:length(uni)){
    sub<-data[data[,idcol] == uni[k],]
    
    #' this line will give the maximum date of survival (mortality date)
    date<-ifelse(is.na(unique(sub[nrow(sub),mortcol])) %in% FALSE,
                 as.character(max(sub[,mortcol], na.rm=T)), NA)
    
    if(!is.na(date)){
      date<-as.Date(date, format = "%Y-%m-%d")
      mort<-1
    }
    
    if(is.na(date)){
      date<-Sys.Date()
      mort<-0
    }
    
    capdate<-as.Date(sub[,capcol], tryFormats = c('%Y-%m-%d', '%m/%d/%Y'), origin = sub[,capcol])
    start<-min(capdate) #' this will be important when multiple capture events occur
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
      
      new.hist$aid<-sub[, idcol][1]
      new.hist$cause<-ifelse(!is.na(new.hist$end), sub[,fatecol][c], NA)
      
      if(nrow(new.hist)>1){
      new.hist$mort<-0
      new.hist[end.row, 'mort']<-mort
      }
      
      if(nrow(new.hist) == 1){
        new.hist$mort<-mort
      }
      #new.hist$sex<-sub$Sex[1]
      #new.hist$spp<-sub$Species[1]
      
      x<-nrow(new.hist)
      new.hist$status<-ifelse(is.na(new.hist$end),0, NA)
      new.hist$status<-ifelse(!is.na(new.hist$end) & new.hist$mort == 0, 0, new.hist$status)
      new.hist$status<-ifelse(!is.na(new.hist$end) & new.hist$mort == 1, 1, new.hist$status)
      new.hist$status<-ifelse(new.hist$status == 1 & new.hist$cause %in% censors, 0, new.hist$status)
      new.hist$Year<-strftime(new.hist$StartDate, format = "%Y")
      
      all.surv<-rbind(new.hist, all.surv)
      
  }

all.surv<-all.surv[!is.na(all.surv$time.alive),]

  if(plot == TRUE){
    surv<-survival::survfit(survival::Surv(time = all.surv$time.alive, event = all.surv$status)~ all.surv$Year)

    summ<-summary(surv)
    cols<-lapply(c(2:6, 8:11), function(x) summ[x])
    tbl<-do.call(data.frame, cols)
    
    
    unistrat<-as.character(unique(tbl$strata))
    
    if(length(unistrat) < length(seq(yearstart:yearend))){
    ex.dat<-data.frame(time = NA, n.risk = NA, n.event = NA, n.censor = NA, surv = 1.0, strata = "all.surv$Year=2019", std.err = NA, lower = NA, upper = NA) 
    tbl<-rbind(ex.dat, tbl)
    }
    
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
  
  
      return(list(all.surv, csurv))
    }
