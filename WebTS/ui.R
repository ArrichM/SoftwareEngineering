toload <- c("shiny","shinydashboard","shinyBS")
toinstall <- toload[which(toload %in% installed.packages()[,1] == F)] #which packages are not already installed?
lapply(toinstall, install.packages, character.only = TRUE) #intall missing packages
lapply(toload, require, character.only = TRUE) #load packages

#import country codes to select from
country_codes <- codelist[which(!is.na(codelist$ioc.name)),c("ioc.name","iso2c")]
country_choices <- as.list(country_codes$iso2c)
names(country_choices) <- country_codes$ioc.name

# Define UI for application that draws a histogram
header <- dashboardHeader(title = "Longoni & Arrich")
sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
  box(
    selectizeInput("target","Please choose the target series",choices = list("Unemployment","Consumption")),
    textInput("keyword","Please enter a keyword you want to use","Please enter a word"),
    #textInput("region","Please choose a region to specify the query to", "AT"),
    selectizeInput("region","Please choose a region to specify the query to", choices = country_choices, selected = "AT"),
    bsButton("start","Go!"),
    plotOutput("google_plot"),
    plotOutput("eurostat_plot")
  )
)

ui <- function(request) dashboardPage(header,sidebar,body)