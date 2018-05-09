library(shiny)
library(plotly)
library(shinythemes)
library(data.table)
library(dslabs)
library(tidyverse)

dis<-us_contagious_diseases

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


ui<-fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage("Disease Exploration", 
             tabPanel("State Level",
                      sidebarPanel(
                        tags$div(
                          tags$p("Choose a state and a disease and see how the number of people with that disease changes over time.")),
                          tags$head(tags$style("#time{height:100vh !important;}")),
                        selectInput(inputId = "state", label = "State", as.list(unique(dis$state))),
                        selectInput(inputId = "disease", label = "Disease", as.list(unique(dis$disease))),
                        #textInput(inputId = "name", label = "Name of Storm", "Name")
                        actionButton("goButton", "Go!"), 
                        width = 2
                      ),
                      mainPanel(
                        plotlyOutput("time"), width = 10
                      )
             )
             #tabPanel("US Level",
              #        sidebarLayout(
               #         sidebarPanel(
                          #tags$div(
                          #  tags$p("Pick a disease and look at how the trends change over time in the US.")),
                          #tags$head(tags$style("#map{height:100vh !important;}"))),
                #        mainPanel(plotOutput("map"), width = 10))
             
  )
)


server<- function(input, output){
  output$time<-renderPlotly({
    input$goButton
    data1<-reactive({dis[dis$disease == isolate(input$disease) & dis$state == isolate(input$state),]})
    d <-  data1()%>%
      accumulate_by(~year)
    p2<- d %>%
      plot_ly(
        x = ~year, 
        y = ~count,
        frame = ~frame, 
        text = ~paste('Count: ', count), 
        color = 'orange',
        hoverinfo = "text",
        type = 'scatter',
        mode = 'lines'
      ) %>%
      layout(
        title = paste(unique(d$disease), 'count over time in', unique(d$state)),
        font = list(color = 'white', size = 12),
        xaxis = list(
          type = "-",
          title = 'Year',
          color = 'white'
        ),
        yaxis = list(
          type = "-",
          title = 'Count',
          color = 'white'
        ),
        paper_bgcolor = 'black',
        plot_bgcolor = 'black'
      )%>%
      animation_opts(
        easing = 'elastic',
        frame = 75, 
        transition = 50, 
        redraw = FALSE
      ) %>%
      animation_slider(
        hide = T
      ) %>%
      animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "bottom", color = 'white'
      )
    p2
  })

}

shinyApp(ui = ui, server = server)
