#This file holds functions taht get called by server.R


google_multiple <- function(tophits,region){
  #this is a function to repeatedly query the google API and combine the answers of the server into one multivariate time series.
  #It takes as input a vector of up to five keywords and will output a ts object with up to five variables#
  
  query_multiple <- lapply(tophits, function(x) gtrends(x, geo = region, gprop = "web", time = "all")[[1]] %>%  
                             dcast(date ~ keyword + geo, value.var = "hits")) #repeatedly query the API using the words in tophits
  #transform output to ts - frequency from google API is allways monthly so we can hardcode this here
  query_multiple_ts <- lapply(query_multiple, function(x) ts(x[,2],start =c(year(x$date[1]),month(x$date[1])), frequency = 12))
  
  #bind ts together to get multivariate ts and set corret colnames
  query_multiple_ts_bound <- do.call(cbind,query_multiple_ts)
  colnames(query_multiple_ts_bound) <- tophits
  return(query_multiple_ts_bound)
}

need_on_exit <- function (expr,FUN, message = paste(label, "must be provided"), label){
  #this function takes an expression and if it is true, it returns NULL. if it is false, it will execute the function specified as FUN
  #and return the message as error message. It is a slightly altered version of shiny::need
  
  force(message)
  if (!isTruthy(expr)){
    FUN()
    return(message)
  }
  else return(invisible(NULL))
}

get_eurounemp <- function(region){
  #this function queries the eurostat API and retrieves the unadjusted series for the unemployment rate of the corresponding region
  
  search_eurostat("unemployment", fixed = F)$code #look up the code of the series: namq_10_gdp
  unemp <- get_eurostat("une_rt_m", select_time = "M") #download monthly series
  #select the not seasonally adjusted series of the selected region, both genders, all ages, in percent:
  unemp <- subset(unemp, geo == region & sex == "T" & unit == "PC_ACT" & age == "TOTAL" & s_adj == "NSA")
  unemp <- label_eurostat(unemp, lang = "de") #asign human-readable labels to the data
  unemp$time <- as.POSIXct.Date(unemp$time) #format time as date
  unemp.ts <- ts(rev(unemp$values), start = c(as.numeric(tail(substr(unemp$time,1,4),1)), #transform to time series object
                                              as.numeric(tail(substr(unemp$time,6,7),1))), freq = 12)
  unemp.ts <- unemp.ts/100 #transform to decimal
  return(unemp.ts)
}

get_eurocons <- function(region){
  #this function queries the eurostat API and retrieves the unadjusted series for the private consumption of the corresponding region
  
  search_eurostat("consumption", fixed = F)$code # get code of series namq_10_fcs
  privcons <- get_eurostat("namq_10_fcs", select_time = "Q") #download quarterly series
  privcons <- subset(privcons, geo == region) #select correct region
  privcons <- label_eurostat(privcons, lang = "de") #assign human readable labels
  privcons <- subset(privcons, unit == "Verkettete Volumen (2010), Millionen Euro" & #not adjusted level values
                       s_adj == "Unbereinigte Daten (d.h. weder saisonbereinigte noch kalenderbereinigte Daten)" &
                       na_item == "Konsumausgaben der privaten Haushalte") #adjust correct series
  privcons$time <- as.POSIXct.Date(privcons$time)  #format time as date
  privcons.ts <- ts(rev(privcons$values), start = c(as.numeric(tail(substr(privcons$time,1,4),1)), #transform to time series object
                                                    as.numeric(tail(substr(privcons$time,6,7),1))), freq = 4)
  #privcons.ts <- diff(privcons.ts, lag = 1)/privcons.ts #transform to quarter to quarter growth
  #use absolute values, suppose cointegration
  return(privcons.ts)
}