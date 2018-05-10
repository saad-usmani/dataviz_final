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
             ),
             tabPanel("US Level",
                     sidebarLayout(
                      sidebarPanel(
             tags$div(
               tags$p("Pick a disease and look at how the trends change over time in the US.")),
             tags$head(tags$style("#map{height:100vh !important;}")),
             sliderInput("year", "Year", min = 1929, max = 2005, value = 1929)),
                     mainPanel(leafletOutput("map"), width = 10)))
             
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
  output$map <- renderLeaflet({
    data3 <- reactive({dis_name[dis_name$disease == input$disease & dis_name$year == input$year,]})
    disease_example<-data3()[,c("NAME", "count")]
    states <- geojsonio::geojson_read("us-states.geojson", what= "sp")
    states@data<-states@data[c(-17),]
    states@data <- merge(states@data, disease_example, by = 'NAME', sort = FALSE)
    m <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>% # set the view to the contiguous United States
      
      # set what the background map should look like.
      #addTiles() # basic
      addProviderTiles("Stamen.Watercolor") #FUN
    
    # what do we have so far
    bins <- c(quantile(na.omit(states$count), seq(0, 1, 1/5)))
    pal <- colorBin("YlOrRd", domain = states$count, bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/>",
      states$count
    ) %>% lapply(htmltools::HTML)
    m %>% 
      addPolygons(
        fillColor = ~pal(states$count),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          testsize = "15px",
          direction = "auto"
        )
      ) %>% 
      addLegend(
        pal = pal, 
        values = ~states$count,
        opacity = 0.7, 
        title="Counts of Disease", 
        position = "bottomright")
    
  })
  
}

shinyApp(ui = ui, server = server)
