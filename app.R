library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(ggmap)

ui<-fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage("Disease Exploration", 
             tabPanel("State Level",
                      sidebarPanel(
                        tags$div(
                          tags$p("Choose a state and a disease and see how the number of people with that disease changes over time.")),
                        tags$head(tags$style("#origin{height:100vh !important;}")),
                        selectInput(inputId = "state", label = "State", as.list(unique(disease$state)),
                        uiOutput('names'),
                        #textInput(inputId = "name", label = "Name of Storm", "Name")
                        actionButton("goButton", "Go!"), 
                        width = 2
                      ),
                      mainPanel(
                        plotOutput("origin"), width = 10
                      )
             ),
             tabPanel("US Level",
                      sidebarLayout(
                        sidebarPanel(
                          tags$div(
                            tags$p("Pick a disease and look at how the trends change over time in the US.")),
                          tags$head(tags$style("#tracks{height:100vh !important;}")))
                        mainPanel(plotlyOutput("tracks"), width = 10))
             )
  )
))


server<- function(input, output){

}

shinyApp(ui = ui, server = server)
