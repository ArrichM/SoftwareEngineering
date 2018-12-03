rm(list = ls()) #clear memory

source("resources.R") #load functions defined in resources.R file

shinyServer(function(input, output, session) {
   
  ###############################################################################
  ################################ REACTIVITY ###################################
  ###############################################################################
  
  disable_start <- observeEvent(input$start, priority = 2,{
    #this reactive deactivates the start button so it cannot be clicked repeatedly, triggering many reevaluations which can possibly take a long time
    updateButton(session, "start", disabled = T) #disable button while query is running to avoid repeated querying
  })
  
  enable_button <- function() updateButton(session, "start", disabled = F) #function to wrap in need_on_exit that will enable the start button again in case of error
  
  
  
  
  # ==========================GOOGLE QUERY=======================================
  
  google_query <- eventReactive(input$start,{ #wait for the user to click the "Go!" button
    
    #this reactive will handle the user inputs related to the google API query and return a multivaritae time series object containing the 
    #popularity over time of the keyword as well as the four words most related to it.
    #The output is a multivariate time series object
    
    #check inputs:
    req(input$keyword, input$region) #require user inputs in order to proceed
    
    keyword <- strsplit(input$keyword, ",")[[1]]  #get keywords to use from user input and save it to variable keyword
    region <- input$region #get region to use from user input and save it to variable region
    
    shiny::validate( #make sure the user entered a maximum of 5 words
      need_on_exit(length(keyword) < 6, enable_button, "Please enter a maximum of 5 words")
    )
    
    #query the google API using the user inputs and save answer to variable named "query"
    query <- gtrends(keyword, geo = region, gprop = "web", time = "all")
    
    
    shiny::validate( #make sure the keyword yields at least any results. if not, enable button and return error message
      need_on_exit(!is.null(query$related_queries), enable_button, "The keyword you entered seems not to be very popular. Please try a different one.")
    )
    
    words_to_add <- max(0,5-length(keyword)) #define how many words should be added automatically
    if(words_to_add > 0) tophits <- c(keyword,query$related_queries$value[1:words_to_add]) #save keywords and fill up to 5 words using the related words
    
    #use function for repeated querying. function can be found in resources.R
    query_ts <- google_multiple(tophits,region)
    
    #the server answers <1 for small values so we have to cut out the < and interpret the remainder as numeric:
    query_ts <- ts(apply(query_ts,2,function(x) as.numeric(gsub("<","",as.character(x)))),start=start(query_ts), frequency = 12)
    
    return(query_ts)
  })
  
  
  
  
  # ==========================EUROSTAT QUERY=====================================
  
  
  eurostat_query <- eventReactive(input$start,{ #wait for the user to click the "Go!" button
    
    #This reactive takes the user input in for of the chosen country and the desired target variable and retrieves the data form eurostat
    #The output is a univariate time series object
    
    
    target <- input$target #read desired target variable from user input
    region <- input$region #read desired region from user input
    
    target_ts <- switch(target, #use switch to select correct target series. the query funcitons can be found in resources.R
                         Unemployment = try(get_eurounemp(region = region)), #wrap with try in order to avoid shutdown in cse of wrong region
                         Consumption = try(get_eurocons(region = region))
                        )
    
    shiny::validate( #make sure we are able to retrieve the a series from eurostat. if not, enable button and return error message
      need_on_exit(!is.null(target_ts), enable_button, "Sorry, the region you chose is not available. Please try a different one.")
    )
    
    return(target_ts) #return the right target series
  })
  
  
  
  
  # ==========================ANALYSIS===========================================
  
  
  analysis <- reactive({
    
    #This reactive takes the two time series objects from the reactives eurostat_query and google_query and performs a PCA on the google data. Then
    #It proceeds fittign a model stepwise by AIC. It returns the final model and the fitted values.
    #The output is  alist object containing the model and the fitted values as a time series object
    
    
    
    target_ts <- eurostat_query() #read outputs of query expreccions
    google_ts <- google_query()
    freq_use <- frequency(target_ts)
    
    #if needed, aggregate google data so the frequency fits the eurostat data
    if(ceiling(frequency(google_ts)) != freq_use) google_ts <- aggregate(google_ts, nfrequency = freq_use)
    
    #do PCA of the time series and get predicted PCs
    google_pca <- prcomp((google_ts))
    comps_ts <- ts(predict(google_pca), start = start(google_ts), frequency = freq_use)

    #create lagged set and set colnames:
    nlags <- isolate(input$nahead):12 #minimal and maximal number of lags to use
    
    comp_lags <- do.call(cbind,lapply(nlags, function(x) lag(comps_ts,-x))) #lag each series for desired number of times and bind them together
    colnames(comp_lags) <- paste0(rep(colnames(comps_ts),length(nlags)),"-L",rep(nlags,each = ncol(comps_ts))) #set correct colnames so no confusion arises afterwards
    
    #cut data into right window
    comp_lags_window <- na.remove(window((comp_lags), end = end(target_ts), start = start(target_ts))) #make sure data is compatible with eurostat data
    target_window <- window(target_ts, start = start(comp_lags_window), end = end(comp_lags_window)) #make sure eurostat data is campatible with google data

    #specify models scopes for stepwise selection
    full_pca <- lm((target_window)~.,data=comp_lags_window) #full model including all lagged series
    empty_pca <- lm((target_window)~1,data=comp_lags_window) #empty model on constant number 1
    
    #run stepwise selection on the lagged PCs selecting according to BIC
    pca_model <- step(empty_pca, scope=list(lower=empty_pca, upper=full_pca) , direction = "both", k = log(ncol(comp_lags_window)))
    
    output$model <- renderPrint({pander(summary(pca_model))}) #write model to output right away
    
    #get fitted values from the model and format them as time series with same start as prediction data
    pca_fitted <- na.remove(ts(predict(pca_model,newdata = comp_lags), start = start(comp_lags),freq = freq_use))
    
    pca <- list(pca_model, pca_fitted) #store outputs in list
    
    updateButton(session, "start", disabled = F) #now that the computation is completed, button is activated again
    
    return(pca) #return the list
  })
  
  
  
  ###############################################################################
  ################################ OUTPUTS ######################################
  ###############################################################################
  
  
  output$google_plot <- renderPlot({ 
    
    #this will plot the google data that is used to create the forecast
    
    
    query_ts <- google_query() #load the google trend series from the output of google_query
    autoplot.zoo(query_ts) #autolpot the series using ggplot2
  })
  
  
  
  output$eurostat_plot <- renderPlot({ 
    
    #this will plot the target series that is retrieved form eurostat
    
    
    target_ts <- eurostat_query() #load the eurostat data from the output of eurostat_query
    autoplot.zoo(target_ts) #autolpot the series using ggplot2
  })
  
  
  
  output$fitted_plot <- renderPlot({
    
    #this will plot the predicted values together with the target series. it is the "final" result of the algorithm
    
    
    pca_fitted <- analysis()[[2]] #read in fitted values from analysis reacrive
    target_ts <- eurostat_query()
    plot(pca_fitted, col = "red", type = "l") #plot the fitted values
    lines(target_ts) #add the target series as comparison
  })


})



