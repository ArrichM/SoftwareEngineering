
google_multiple <- function(tophits,region){
  #this is a function to repeatedly query the google API and combine the answers of the server into one multivariate time series.
  #It takes as input a vector of up to five keywords and will output a ts object with up to five variables#
  
  query_multiple <- lapply(tophits, function(x) gtrends(x, geo = region, gprop = "web", time = "all")[[1]] %>%  
                             dcast(date ~ keyword + geo, value.var = "hits")) #repeatedly query the API using the word in tophits
  #transform output to ts - frequency from google API is allways monthly so we can hardcode this here
  query_multiple_ts <- lapply(query_multiple, function(x) ts(x[,2],start =c(year(x$date[1]),month(x$date[1])), frequency = 12))
  
  #bind ts together to get multivariate ts and set corret colnames
  query_multiple_ts_bound <- do.call(cbind,query_multiple_ts)
  colnames(query_multiple_ts_bound) <- tophits
  return(query_multiple_ts_bound)
}

need_on_exit <- function (expr,FUN, message = paste(label, "must be provided"), label){
  #this function takes an expression and if it is true, it returns NULL. if it is false, it will execute the function specified with as FUN
  #and return the message as error message. It is a slightly altered version of shiny::need
  force(message)
  if (!isTruthy(expr)){
    FUN()
    return(message)
  }
  else return(invisible(NULL))
}
