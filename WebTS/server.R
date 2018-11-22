rm(list = ls()) #clear memory

toload <- c("shiny","shinydashboard","gtrendsR", "magrittr","reshape2","lubridate","zoo","tseries","eurostat","dyn","ggplot2","ggfortify",
            "shinyBS","countrycode")
toinstall <- toload[which(toload %in% installed.packages()[,1] == F)] #which packages are not already installed?
lapply(toinstall, install.packages, character.only = TRUE) #intall missing packages
lapply(toload, require, character.only = TRUE) #load packages

source("resources.R") #load functions defined in resources.R file

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  ###############
  ##Reactivity###
  ###############
  
  google_query <- eventReactive(input$start,{#wait for the user to click the "Go!" button
    #this reactive will handle the user inputs related to the google API query and return a multivaritae time series object containing the 
    #popularity over time of the keyword as well as the four words most relatrd to it.
    
    req(input$keyword, input$region) #require user inputs in order to proceed
    
    updateButton(session, "start", disabled = T) #disable button while query is running to avoid repeated querying

    keyword <- input$keyword #get keyword to use from user input and save it to variable keyword
    region <- input$region #get region to use from user input and save it to variable region

    #query the google API using the user inputs and save answer to variable named "query"
    query <- gtrends(keyword, geo = region, gprop = "web", time = "all")
    
    enable_button <- function() updateButton(session, "start", disabled = F) #in case of error, enable button again, see next lines
    
    shiny::validate( #make sure the keyword yields at least any results. if not, enable button and return error message
      need_on_exit(!is.null(query$interest_over_time), enable_button, "The keyword you entered seems not to be very popular. Please try a different one.")
    )
    
    tophits <- c(keyword,query$related_queries$value[1:4]) #save keyword itself and top 4 related words - 5 words in total
    
    #use function for repeated querying. function can be found in resources.R
    query_ts <- google_multiple(tophits,region)
    
    #the server answers <1 for small values so we have to cut out the < and interpret the remainder as numeric:
    query_ts <- ts(apply(query_ts,2,function(x) as.numeric(gsub("<","",as.character(x)))),start=start(query_ts), frequency = 12)
    
    updateButton(session, "start", disabled = F) #now that the query is completed, button is activated again
    
    return(query_ts)
  })
  
  eurostat_query <- eventReactive(input$start,{
    target <- input$target #read desired target variable from user input
    region <- input$region #read desired region from user input
    
    target_ts <- switch(target, #use switch to select correct target series. the query funcitons can be found in resources.R
                         Unemployment = get_eurounemp(region = region),
                         Consumption = get_eurocons(region = region)
                        )
    return(target_ts) #return the right target series
  })
  
  analysis <- reactive({
    target_ts <- eurostat_query() #read outputs of query expreccions
    google_ts <- google_query()
    freq_use <- frequency(target_ts)
    
    #if needed, aggregate google data so the frequency fits the eurostat data
    if(ceiling(frequency(google_ts)) != freq_use) google_ts <- aggregate(google_ts, nfrequency = freq_use)
    
    #do PCA of the time series and get predicted PCs
    google_pca <- prcomp((google_ts))
    comps_ts <- ts(predict(google_pca), start = start(google_ts), frequency = freq_use)

    #create lagged set and set colnames:
    nlags <- 0:12 #maximal number of lags to use
    
    comp_lags <- do.call(cbind,lapply(nlags, function(x) lag(comps_ts,-x))) #lag each series for desired number of times and bind them together
    colnames(comp_lags) <- paste0(rep(colnames(comps_ts),length(nlags)),"-L",rep(nlags,each = ncol(comps_ts))) #set correct colnames so no confusion arises afterwards
    
    #cut data into right window
    comp_lags_window <- na.remove(window((comp_lags), end = end(target_ts), start = start(target_ts))) #make sure data is campatible with eurostat data
    target_window <- window(target_ts, start = start(comp_lags_window), end = end(comp_lags_window)) #make sure eurostat data is campatible with google data

    #specify models scopes for stepwise selection
    full_pca <- lm((target_window)~.,data=comp_lags_window) #full model including all lagged series
    empty_pca <- lm((target_window)~1,data=comp_lags_window) #empty model on constant number 1
    
    #run stepwise selection on the lagged PCs selecting according to BIC
    pca_model <- step(empty_pca, scope=list(lower=empty_pca, upper=full_pca) , direction = "both", k = log(ncol(comp_lags_window)))
    
    #get fitted values from the model and format them as time series with same start as prediction data
    pca_fitted <- na.remove(ts(predict(pca_model,newdata = comp_lags), start = start(comp_lags),freq = freq_use))
    
    pca <- list(pca_model, pca_fitted) #store outputs in list
    
    return(pca) #return the list
  })
  
  ############
  ##Outputs###
  ############
  
  output$google_plot <- renderPlot({ #define plot as output to display to the user
    query_ts <- google_query() #load the google trend series from the output of google_query
    autoplot.zoo(query_ts) #autolpot the series using ggplot2
  })
  output$eurostat_plot <- renderPlot({ #define plot as output to display to the user
    target_ts <- eurostat_query() #load the eurostat data from the output of eurostat_query
    autoplot.zoo(target_ts) #autolpot the series using ggplot2
  })
  output$fitted_plot <- renderPlot({
    pca_fitted <- analysis()[[2]] #read in fitted values from analysis reacrive
    target_ts <- eurostat_query()
    plot(pca_fitted, col = "red", type = "l") #plot the fitted values
    lines(target_ts) #add the target series as comparison
  })
  
})



