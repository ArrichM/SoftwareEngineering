toload <- c("shiny","shinydashboard","shinyBS")
toinstall <- toload[which(toload %in% installed.packages()[,1] == F)] #which packages are not already installed?
lapply(toinstall, install.packages, character.only = TRUE) #intall missing packages
lapply(toload, require, character.only = TRUE) #load packages

# #import country codes to select from
# country_codes <- codelist[which(!is.na(codelist$ioc.name)),c("ioc.name","iso2c")]
# country_choices <- as.list(country_codes$iso2c)
# names(country_choices) <- country_codes$ioc.name

# Define UI for application that draws a histogram
header <- dashboardHeader(title = "Longoni & Arrich")
sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
  box(
    textInput("keyword","Please enter a keyword you want to use","Please enter a word"),
    textInput("region","Please choose a region to specify the query to", "AT"),
    plotOutput("query_plot"),
    bsButton("start","Go!", disabled = F)
  )
)

ui <- function(request) dashboardPage(header,sidebar,body)