library(dslabs)
library(plotly)
library(tidyverse)
dis<-us_contagious_diseases
murder<-murders

disease_example <- disease %>%
  filter(disease == "Measles" & state == 'Florida')

p <- plot_ly(x = ~disease_example$year, y = ~disease_example$count, type = 'scatter',
             frame = ~frame)

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
    split = ~disease,
    frame = ~frame, 
    text = ~paste('Count: ', count), 
    color = 'orange',
    hoverinfo = "text",
    type = 'scatter',
    mode = 'lines',
    showLegend = FALSE,
    frame.plot=FALSE,
    bty = 'n'
  ) %>%
  layout(
    title = paste(unique(d$disease), 'count over time in', unique(d$state)),
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

dis_sum <- dis %>%
  group_by(disease, year) %>%
  summarise(total = sum(count)) %>%
  filter(disease == 'Measles') %>%
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
