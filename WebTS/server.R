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
    query_ts <- ts(apply(query_ts,2,function(x) as.numeric(gsub("<","",as.character(x)))),start=start(query_ts))
    
    updateButton(session, "start", disabled = F) #now that the query is completed, button is activated again
    
    return(query_ts)
  })
  
  
  ############
  ##Outputs###
  ############
  
  output$query_plot <- renderPlot({
    query_ts <- google_query()
    
    autoplot.zoo(query_ts)
  })
  
})



