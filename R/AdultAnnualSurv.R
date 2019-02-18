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
#' @param cols chracter vector of colors to plot bars in barplot
#' @param title desired title of survival plot (character)
#' @return Returns a data.frame with animal ID, start date of modeling, end date of modeling, status of animal (alive = 0, dead = 1), and number of months alive during time period
#' @keywords adult, annual, survival, kaplan-meier, analysis
#' @export
#' @examples
#' \donttest{AdultSurv<-AdultAnnualSurv(data = yourdata, uni = uniquevector, mortcol = "MortalityDate", yearstart = 2015, yearend = 2019 , cause = "CaptureMort")}
#'

AdultAnnualSurv<-function(data, format, uaidcol,capcol, mortcol, yearstart, yearend, cause, plot, cols,title){
  data[,mortcol]<-as.Date(data[,mortcol], format = format)
  Year<-yearstart:yearend
  hist<-data.frame(Year = Year, StartDate = paste("01/01/", Year, sep = ""), EndDate = paste("12/31/", Year, sep = ""))
  hist$StartDate<-as.Date(hist$StartDate, format = "%m/%d/%Y")
  hist$EndDate<-as.Date(hist$EndDate, format = "%m/%d/%Y")

  d<-data.frame()
  z<-data.frame()

  uni<-unique(data[,uaidcol])

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
    capdate<-as.Date(sub[,capcol], format = format, origin = sub[,capcol])
    x<-nrow(hist)

    for(l in 1:x){
      xxx<-hist[l,]
      m<-ifelse(date >= xxx[,2] & date <= xxx[,3], as.character(difftime(date, xxx[,2], units = "days")/30.4375), 12)
      m<-as.numeric(m)
      rem<-ifelse(capdate >= xxx[,2] & capdate <= xxx[,3], 0, 1)
      c<-length(capdate)
      Yr<-strftime(xxx[,2], format = "%Y")
      remove<-ifelse(sum(rem) == c & Yr != yearend ,1, 0)
      Stat<-ifelse(m == 12, 0,
                   ifelse(m < 12 & date == Sys.Date(), 0, 1))
      All<-data.frame(AID = sub$UAID[1], Year = strftime(xxx[,2], format = "%Y"),
                      StartDate = xxx[,2], EndDate = xxx[,3], Time= m, Remove = remove,
                      Status = ifelse(Stat == 1 & sub$X %in% cause, 0, Stat))

      d<-rbind(d, All)
    }
    z<-rbind(d, z)
    z<-z[!duplicated(z[,c(1:4)]),]
}

###### Remove rest of rows after an animal dies or is censored #######
  u<-unique(z$AID)
  r<-data.frame()
  fin<-data.frame()
  e<-data.frame()
    for(m in 1:length(u)){
      sub<-z[z$AID == u[m],]

    #for(p in 1:nrow(sub)){
      if(sum(sub$Status) == 0){
        e<-sub
      }
    if(sum(sub$Status) > 0){
      g<-which(sub$Status == 1)
      y<-sub[1:g,]
    }

    r<-rbind(e, y)
    fin<-rbind(r, fin)
    fin<-fin[!duplicated(fin[,c(1:4)]),]
    fin<-fin[fin$Remove == 0,]
    #}
    }
  if(plot == TRUE){
    surv<-survival::survfit(survival::Surv(time = s$Time, event = s$Status)~ s$Year)

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
    barplot(csurv$Surv, col = c('red', 'blue', 'green', 'yellow', 'purple'), ylim = c(0,1), names.arg = csurv$Year, border = NA, main = title)
  }
      return(fin)
    }
