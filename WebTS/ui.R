toload <- c("shiny","shinydashboard","gtrendsR", "magrittr","reshape2","lubridate","zoo","tseries","eurostat","dyn","ggplot2","ggfortify",
            "shinyBS","countrycode","pander")
toinstall <- toload[which(toload %in% installed.packages()[,1] == F)] #which packages are not already installed?
lapply(toinstall, install.packages, character.only = TRUE) #install missing packages
lapply(toload, require, character.only = TRUE) #load packages

# import country codes which are available on eurostat
load("data/eurostat_countries.Rdata")

# load general list of country codes and english coutry names
country_codes <- codelist[which(!is.na(codelist$ioc.name)),c("ioc.name","eurostat")]

# select those which are available on the eursotat api
country_codes <- country_codes[which(country_codes$eurostat %in% eurostat_countries),]

# transform to list
country_choices <- as.list(country_codes$eurostat)
names(country_choices) <- country_codes$ioc.name


# Define UI for application
header <- dashboardHeader(title = "Longoni & Arrich")
sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
  box(
    
    ###########INPUTS##############
    
    selectizeInput("target","Please choose the target series",choices = list("Unemployment","Consumption")),
    textInput("keyword","Please enter up to five keywords you want to use","Please enter a word"),
    selectizeInput("region","Please choose a region to specify the query to", choices = country_choices, selected = "AT"),
    sliderInput("nahead" ,"Number of minimal leading periods", 0, 11, value = c(1,11)),
    checkboxInput("end_difflog","Log and differentiate target series?"),
    checkboxInput("exo_difflog","Log and differentiate covariate series?"),
    bsButton("start","Go!"),
    
    ###########OUTPUTS#############
    
    plotOutput("google_plot"),
    plotOutput("eurostat_plot")
  ),
  box(
    plotOutput("fitted_plot"),
    verbatimTextOutput("model")
  )
)

ui <- function(request) dashboardPage(header,sidebar,body)
