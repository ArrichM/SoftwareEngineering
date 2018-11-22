toload <- c("shiny","shinydashboard","shinyBS")
toinstall <- toload[which(toload %in% installed.packages()[,1] == F)] #which packages are not already installed?
lapply(toinstall, install.packages, character.only = TRUE) #intall missing packages
lapply(toload, require, character.only = TRUE) #load packages


# Define UI for application that draws a histogram
header <- dashboardHeader(title = "Longoni & Arrich")
sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
  box(
    textInput("keyword","Please enter a keyword you want to use","Please enter a word"),
    textInput("region","Please enter a region you want to use","Please enter a region"),
    plotOutput("query_plot"),
    bsButton("start","Go!")
  )
)

ui <- function(request) dashboardPage(header,sidebar,body)
