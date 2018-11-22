toload <- c("shiny","shinydashboard","gtrendsR", "magrittr","reshape2","lubridate","zoo","tseries","eurostat","dyn","ggplot2","ggfortify", "shinyBS")
toinstall <- toload[which(toload %in% installed.packages()[,1] == F)] #which packages are not already installed?
lapply(toinstall, install.packages, character.only = TRUE) #intall missing packages
lapply(toload, require, character.only = TRUE) #load packages

source("resources.R") #load functions defined in resources.R file

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  ###############
  ##Reactivity###
  ###############
  
  google_query <- reactive({
    #this reactive will handle the user inputs related to the google API query and return a multivaritae time series object containing the 
    #popularity over time of the keyword as well as the four words most relatrd to it.
    
    input$start #wait for the user to click the "Go!" button
    
    req(input$keyword, input$region) #require user inputs on order to proceed
    
    #get user inputs but isolate them so no reaction will be triggered everytime the user types a character. 
    #instead, reaction should only be triggered by clicking the "go!" button:
    keyword <- isolate(input$keyword) #get keyword to use from user input and save it to variable keyword
    region <- isolate(input$region) #get region to use from user input and save it to variable region
    
    #query the google API using the user inputs and save answer to variable named "query"
    query <- gtrends(keyword, geo = region, gprop = "web", time = "all") 
    
    tophits <- c(keyword,query$related_queries$value[1:4]) #save keyword itsleve and top 4 related words - 5 words in total
    
    #use function for repeated querying. function can be found in resources.R
    query_ts <- google_multiple(tophits)
    
    #the server answers <1 for small values so we have to cut out the < and interpret the remainder as numeric:
    query_ts <- ts(apply(query_ts,2,function(x) as.numeric(gsub("<","",as.character(x)))),start=start(query_ts))
    
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
