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
get.eurounemp <- function(){
  search_eurostat("Arbeitslosengeld", fixed = F)$code #namq_10_gdp
  unemp <- get_eurostat("une_rt_m", select_time = "M")
  unemp <- subset(unemp, geo == "AT" & sex == "T" & unit == "PC_ACT" & age == "TOTAL" & s_adj == "NSA")
  unemp <- label_eurostat(unemp, lang = "de")
  unemp$time <- as.POSIXct.Date(unemp$time)
  unemp.ts <- ts(rev(unemp$values), start = c(as.numeric(tail(substr(unemp$time,1,4),1)),
                                              as.numeric(tail(substr(unemp$time,6,7),1))), freq = 12)
  unemp.ts <- unemp.ts/100
  return(unemp.ts)
}
get.eurocons <- function(){
  search_eurostat("consumption", fixed = F)$code #namq_10_fcs
  privcons <- get_eurostat("namq_10_fcs", select_time = "Q")
  privcons <- subset(privcons, geo == "AT")
  privcons <- label_eurostat(privcons, lang = "de")
  privcons <- subset(privcons, unit == "Verkettete Volumen (2010), Millionen Euro" & s_adj == "Saison- und kalenderbereinigte Daten" &
                       na_item == "Konsumausgaben der privaten Haushalte")
  privcons$time <- as.POSIXct.Date(privcons$time)
  privcons.ts <- ts(rev(privcons$values), start = c(as.numeric(tail(substr(privcons$time,1,4),1)),
                                                    as.numeric(tail(substr(privcons$time,6,7),1))), freq = 4)
  privcons.ts <- diff(privcons.ts, lag = 4)/privcons.ts
  return(privcons.ts)
}
freq_use <- 12L
#query the google api for keywords
query <- gtrends(c("Arbeitslosengeld"),geo = "AT", gprop = "web", time = "all")
#query2 <-  gtrends(c("Zalando"),geo = "AT", gprop = "web", time = "all")
tophits <- query$related_queries$value[1:5]
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

query_multiple_ts_bound <- ts(apply(query_multiple_ts_bound,2,function(x) as.numeric(gsub("<","",as.character(x)))),start=start(query_multiple_ts_bound))

#do PCA of the time series and get predicted PCs
PCA_query <- prcomp((query_multiple_ts_bound)) #insert diff here if differentiation is desired
comps_ts <- ts(predict(PCA_query), start = start(query_multiple_ts_bound), frequency = 12)
plot(comps_ts)

#aggregate the pca of the google query to rigth frequency, i.e. same freq as the target series
if(frequency(comps_ts) != freq_use) comps_ts <- aggregate(comps_ts,nfrequency = freq_use)

#load target series from eurostat api
unemp = get.eurounemp()
#unemp = get.eurocons()

#create lagged set and set colnames
nlags <- 1:1

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
pca.model <- step(empty.pca, scope=list(lower=empty.pca, upper=full.pca) , direction = "both")
summary(pca.model)

#produce fitted values
point_estimate <- na.remove(ts(predict(pca.model,newdata = comp_lags), start = start(comp_lags),freq = freq_use))
plot(point_estimate)
lines(unemp, col = "red")

plot((pexp(rexp(1000,1))))




