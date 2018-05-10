library(dslabs)
library(plotly)
library(tidyverse)
library(install.load)
library(dplyr)
library(geojsonio)
library(downloader)
library(leaflet)
library(maptools)

#Importing the dataset
dis<-us_contagious_diseases

dis_name<-us_contagious_diseases
colnames(dis_name)[2]<-"NAME"

#Attempting to create first time series plot with plotly

p <- plot_ly(x = ~disease_example$year, y = ~disease_example$count, type = 'scatter',
             frame = ~frame)

#Creating animated time series by state and disease

disease_example <- dis %>%
  filter(disease == "Measles" & state == 'Florida')


#Creating function to get lines in animation (taken from Plotly website)
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

d <-  disease_example%>%
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



#Attempting to create summary of time series by year and disease animation but
#ultimately failed doing this
dis_sum <- dis %>%
  group_by(disease, year) %>%
  summarise(total = sum(count)) %>%
  filter(disease == 'Hepatitis A') %>%
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

#First attempt at choropleth using javascript and datamap from Github

#Had to first transform the data

dis2 <- transform(dis,
                   state = state.abb[match(as.character(state), state.name)],
                   fillKey = cut(count, unique(quantile(count, seq(0, 1, 1/5)), labels = LETTERS[1:5]))
)

disease_2 <- dis2 %>%
  filter(disease == "Measles")

#Get fills

fills = setNames(
  c(RColorBrewer::brewer.pal(5, 'YlOrRd'), 'white'),
  c(LETTERS[1:4], 'defaultFill')
)

#Create JSON arrays

disease_ex2 <- dlply(na.omit(dis2), "year", function(x){
  y = toJSONArray2(x, json = F)
  names(y) = lapply(y, '[[', 'state')
  return(y)
})

# Create map
options(rcharts.cdn = TRUE)
map <- Datamaps$new()
map$set(
  dom = 'chart_1',
  scope = 'usa',
  fills = fills,
  data = disease_ex2[[1]],
  legend = TRUE,
  labels = TRUE
)
map

#Primitive example worked

#Tried ichoropleth and it works in Rstudio but I didn't realize 
#that it is not compatible with Shiny so I wasted many hours on this

source('ichoropleth.R')
ichoropleth(count ~ state,
            data = disease_2,
            pal = 'Blues',
            ncuts = 5,
            animate = 'year',
            play = F
)

#Now attempting to use choropleth from Leaflet - most of this taken from
#our lab I worked on

disease_example <- dis_name %>%
  filter(disease == "Measles" & year == '1928') %>%
  select(NAME, count)

u <- "eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json"
#downloader::download(url = u, destfile="us-states.geojson")

# use geojsonio to load the spatial data into sp objects
disease_example <- dis_name %>%
  filter(disease == "Measles" & year == '1980') %>%
  select(NAME, count)
states <- geojsonio::geojson_read("us-states.geojson", what= "sp")
states@data<-states@data[c(-17),]
states@data <- merge(states@data, disease_example, by = 'NAME', sort = FALSE)
m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>% # set the view to the contiguous United States
  
  # set what the background map should look like.
  #addTiles() # basic
  addProviderTiles("Stamen.Watercolor") #FUN

# what do we have so far
bins <- c(0,300,900,1200,1500,1800,2100,2400,2700,Inf)
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

#Yay, it works. 
