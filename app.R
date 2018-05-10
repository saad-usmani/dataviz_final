library(shiny)
library(plotly)
library(shinythemes)
library(data.table)
library(dslabs)
library(tidyverse)
library(geojsonio)
library(downloader)
library(leaflet)
library(maptools)

dis<-us_contagious_diseases
dis_name<-us_contagious_diseases
colnames(dis_name)[2]<-"NAME"
bins <- c(0,300,900,1200,1500,1800,2100,2400,2700,Inf)

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
                          tags$p("Choose a disease and a state combo and see how the number of people with that disease changes over time.")),
                        tags$head(tags$style("#time{height:100vh !important;}")),
                        selectInput(inputId = "disease", label = "Disease", as.list(unique(dis$disease))),
                        selectInput(inputId = "state", label = "State", as.list(unique(dis$state))),
                        #textInput(inputId = "name", label = "Name of Storm", "Name")
                        actionButton("goButton", "Go!"), 
                        width = 2
                      ),
                      mainPanel(
                        plotlyOutput("time"), width = 10
                      )
             ),
             tabPanel("Overall Count",
                      sidebarPanel(
                        tags$div(
                          tags$p("An overall trend from all states of the disease chosen from the previous tab")),
                        tags$head(tags$style("#time{height:100vh !important;}")),
                        actionButton("goButton", "Go!"), 
                        width = 2
                      ),
                      mainPanel(
                        plotlyOutput("overall"), width = 10
                      )
             ),
             tabPanel("US Level",
                     sidebarLayout(
                      sidebarPanel(
             tags$div(
               tags$p("Using the disease from the previous tab, pick a year and look at how the trends change over time in the US. Note: You might see an error because this choropleth map takes a bit longer to load, so press Go!, play around with the slider, be patient, and have fun!")),
             tags$head(tags$style("#map{height:100vh !important;}")),
             uiOutput('year'),
             actionButton("go2", "Go!")),
                     mainPanel(leafletOutput("map"), width = 8)))
             
  )
)


server<- function(input, output){
  output$year<-renderUI({
    data4 <- reactive({dis_name[dis_name$disease ==input$disease,]})
    data5 <- reactive({min(data4()$year)})
    data6 <- reactive({max(data4()$year)})
    sliderInput("year", "Year:", min = data5(), max = data6(), value = 1980)
  })
  
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
  
  output$overall <- renderPlotly({
    dis_sum <- dis %>%
      group_by(disease, year) %>%
      summarise(total = sum(count)) %>%
      filter(disease == input$disease) %>%
      data.frame()
    
    
    d3<-dis_sum%>%
      accumulate_by(~year)
    
    
    p3<- d3 %>%
      plot_ly(
        x = ~year, 
        y = ~total,
        frame = ~frame, 
        text = ~paste('Count: ', total), 
        color = 'orange',
        hoverinfo = "text",
        type = 'scatter',
        mode = 'lines'
      ) %>%
      layout(
        title = paste(unique(dis_sum$disease), 'overall count over time'),
        font = list(color = 'white', size = 12, font = ''),
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
    p3
    
  })
  
  output$map <- renderLeaflet({
    input$go2
    data3 <- reactive({dis_name[dis_name$disease == input$disease & dis_name$year == input$year,]})
    disease_example<-data3()[,c("NAME", "count")]
    states <- geojsonio::geojson_read("us-states.geojson", what= "sp")
    states@data<-states@data[c(-17),]
    states@data <- merge(states@data, disease_example, by = 'NAME', sort = FALSE)
    m <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>% # set the view to the contiguous United States
      
      # set what the background map should look like.
      #addTiles() # basic
      addProviderTiles("CartoDB.DarkMatter") #FUN
    
    # what do we have so far
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
