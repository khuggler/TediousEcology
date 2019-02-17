#' @title Conversion of adult capture data into the correct format for modeling annual survival
#
#' @description Convert capture and cause-specific mortality data of adults into the proper format for conducting survival analysis.This function will convert raw data
#' that contains start dates and mortality dates of individual animals and transform it into the proper format required by package survival
#' to model annual survival.
#' @param data data.frame that contains a column of unique animal identifier, start date, mortality date, and cause of mortality
#' @param format format of mortcol
#' @param uaidcol name of column where unique animal identifiers are located
#' @param mortcol name of column that contains the date of mortality
#' @param yearstart year desired to begin modeling survival (e.g. beginning year of study)
#' @param yearend year desired to end modeling survival
#' @param cause vector of causes that require censoring (e.g. collar_failure, capturemort, etc.)
#' @return Returns a data.frame with animal ID, start date of modeling, end date of modeling, status of animal (alive = 0, dead = 1), and number of months alive during time period
#' @keywords adult, annual, survival, kaplan-meier, analysis
#' @export
#' @examples
#' \donttest{AdultSurv<-AdultAnnualSurv(data = yourdata, uni = uniquevector, mortcol = "MortalityDate", yearstart = 2015, yearend = 2019 , cause = "CaptureMort")}
#'

AdultAnnualSurv<-function(data, format, uaidcol, mortcol, yearstart, yearend, cause){
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
    date<-as.Date(max(sub[,mortcol], na.rm=T), format = format, origin = sub[,mortcol])

    x<-nrow(hist)

    for(l in 1:x){
      xxx<-hist[l,]
      m<-ifelse(date >= xxx[,2] & date <= xxx[,3], as.character(difftime(date, xxx[,2], units = "days")/30.4375), 12)
      m<-as.numeric(m)
      Stat<-ifelse(m ==12, 0, 1)
      All<-data.frame(AID = sub$UAID[1], Year = strftime(xxx[,2], format = "%Y"),
                      StartDate = xxx[,2], EndDate = xxx[,3], Time= m,
                      Status = ifelse(Stat == 1 & sub$X %in% cause, 0, Stat))

      d<-rbind(d, All)
    }
    z<-rbind(d, z)
    z<-z[!duplicated(z[,c(1:4)]),]

    ###### Remove rest of rows after an animal dies
  u<-unique(z$AID)

  r<-data.frame()

    for(k in 1:length(u)){
      sub<-z[z$AID == u[k],]

    for(p in 1:nrow(sub)){
      if(sub[p,]$Status == 1){
        sub<-sub[1:p,]
      }
      if(sub[p,]$Status == 0){
        sub<-sub

      }
    r<-rbind(sub, r)
    }
      }
    }
      return(r)
    }
