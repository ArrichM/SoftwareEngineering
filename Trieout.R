library(gtrendsR)
library(reshape2)
library(lubridate)
library(zoo)
library(eurostat)
library(dyn)
library(magrittr)
library(xts)
library(stats)
library(tseries)

stdz <- function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T)

log_exogenous_yes <- 1 #0 or 1 for log and diffing
log_endogenous_yes <- 1

hitword <- "amazon"
series_use <- 2 #2 for cons, 1 for unemp
region_use <- "DE"

get_eurounemp <- function(){
  search_eurostat("unemployment", fixed = F)$code #namq_10_gdp
  unemp <- get_eurostat("une_rt_m", select_time = "M")
  unemp <- subset(unemp, geo == "AT" & sex == "T" & unit == "PC_ACT" & age == "TOTAL" & s_adj == "NSA")
  unemp <- label_eurostat(unemp, lang = "de")
  unemp$time <- as.POSIXct.Date(unemp$time)
  unemp.ts <- ts(rev(unemp$values), start = c(as.numeric(tail(substr(unemp$time,1,4),1)),
                                              as.numeric(tail(substr(unemp$time,6,7),1))), freq = 12)
  unemp.ts <- unemp.ts/100
  return(unemp.ts)
}

get_eurocons <- function(region=region_use){
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

freq_use <- 4 #unemp has 12, cons has 4
#query the google api for keywords

query <- gtrends(c(hitword),geo = "AT", gprop = "web", time = "all")
#query2 <-  gtrends(c("Zalando"),geo = "AT", gprop = "web", time = "all")
tophits <- query$related_queries$value[1:5]
tophits
#tophits2 <- query2$related_queries$value[1:5]

#get series for keywords separately to get full granity of each series
google_multiple <- function(tophits){
  query_multiple <- lapply(tophits, function(x) gtrends(x,geo = "AT", gprop = "web", time = "all")[[1]] %>%  
                             dcast(date ~ keyword + geo, value.var = "hits"))
  #transform output to ts
  query_multiple_ts <- lapply(query_multiple, function(x) ts(x[,2],start =c(year(x$date[1]),month(x$date[1])), frequency = 12))
  
  #bind ts together to get multivariate ts and set corret colnames
  query_multiple_ts_bound <- do.call(cbind,query_multiple_ts)
  colnames(query_multiple_ts_bound) <- tophits
  return(query_multiple_ts_bound)
}

#query_multiple_ts_bound <- cbind(google_multiple(tophits),google_multiple(tophits2))
query_multiple_ts_bound <- google_multiple(tophits)

query_multiple_ts_bound <- ts(apply(query_multiple_ts_bound,2,function(x) as.numeric(gsub("<","",as.character(x)))),
                              start=start(query_multiple_ts_bound), freq = 12)

autoplot.zoo(query_multiple_ts_bound)

#aggregate the pca of the google query to rigth frequency, i.e. same freq as the target series
if(frequency(query_multiple_ts_bound) != freq_use) query_multiple_ts_bound <- aggregate(query_multiple_ts_bound,nfrequency = freq_use)

#log diffs if desired
if(log_exogenous_yes == 1) query_multiple_ts_bound <- diff(log(1+query_multiple_ts_bound))

#do PCA of the time series and get predicted PCs
PCA_query <- prcomp((query_multiple_ts_bound)) #insert diff here if differentiation is desired
comps_ts <- ts(predict(PCA_query), start = start(query_multiple_ts_bound), frequency = freq_use)
plot(comps_ts)


#load target series from eurostat api
if(series_use == 1) unemp <- get_eurounemp()
if(series_use == 2) unemp <- get_eurocons()

unemp_orig <- unemp

if(log_endogenous_yes == 1) unemp <- diff(log(unemp))

#create lagged set and set colnames
nlags <- 0:12

comp_lags <- do.call(cbind,lapply(nlags, function(x) lag(comps_ts,-x)))
colnames(comp_lags) <- paste0(rep(colnames(comps_ts),length(nlags)),"-L",rep(nlags,each = 5))

#cut data into right window
comp_lags_window <- na.remove(window((comp_lags), end = end(unemp)))
unemp_window <- window(unemp, start = start(comp_lags_window))
end(comp_lags_window)

#specify models scopes for stepwise selection
full.pca <- lm((unemp_window)~.,data=comp_lags_window)
empty.pca <- lm((unemp_window)~1,data=comp_lags_window)

#run stepwise selection on the lagged PCs
pca.model <- step(empty.pca, scope=list(lower=empty.pca, upper=full.pca) , direction = "both", k = 6.2)
summary(pca.model)

#produce fitted values
point_estimate <- na.remove(ts(predict(pca.model,newdata = comp_lags), start = start(comp_lags),freq = freq_use))
plot(unemp)
lines(point_estimate, col = "red")


#if logged, trace back level values


for(i in 1:length(nahead)){

  window(prediction[,1],start = nahead[i+1], end = nahead[i+1]) <- as.numeric(window(prediction[,1],start = nahead[i], end = nahead[i])) *
    (1+as.numeric(window(prediction[,2],start = nahead[i+1], end = nahead[i+1])))
}

pred_orig <- prediction
time_total <- time(unemp_orig)


multiply_recursive <- function(original,growth_rates){
  
  # This function takes a level series and a series of groth rates that may be longer than the level sereis. It 
  # retruns the level series that results when applying the growth values of period t to the level of period and interpreting the result
  # as level of t+1.
  
  nahead <- time(window(growth_rates, start = end(original)))
  prediction <- cbind(original,growth_rates)
  pred_orig <- na.remove(prediction)
  time_total <- time(pred_orig)
  
  for(i in 1:(length(nahead)-1)){
    
    window(prediction[,1],start = nahead[i+1], end = nahead[i+1]) <- as.numeric(window(prediction[,1],start = nahead[i], end = nahead[i])) *
      (1+as.numeric(window(prediction[,2],start = nahead[i+1], end = nahead[i+1])))
  }
  
  for(i in 1:(length(time_total)-1)){
    window(pred_orig[,1],start = time_total[i+1], end = time_total[i+1]) <- as.numeric(window(prediction[,1],start = time_total[i], end = time_total[i])) *
      (1+as.numeric(window(prediction[,2],start = time_total[i+1], end = time_total[i+1])))
  }
  
  forecast_in_levels <- ts(append(pred_orig[,1],window(prediction[,1], start = nahead[2])), end=end(prediction), freq = frequency(prediction))
  
  return(forecast_in_levels)
  
}

multiply_recursive(unemp_orig,point_estimate)

plot(pred_orig[,1])

lines(unemp_orig, col = "red")


