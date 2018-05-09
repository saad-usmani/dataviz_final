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
    text = ~state, 
    color = 'red',
    hoverinfo = "text",
    type = 'scatter',
    mode = 'lines',
    showLegend = FALSE
  ) %>%
  layout(
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
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )
