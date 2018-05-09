library(dslabs)
library(plotly)
library(tidyverse)
dis<-us_contagious_diseases
murder<-murders

dis2 <- transform(dis,
                   state = state.abb[match(as.character(state), state.name)],
                   fillKey = cut(count, unique(quantile(count, seq(0, 1, 1/5)), labels = LETTERS[1:5]))
)
kable(head(datm2), format = 'html', table.attr = "class=nofluid")

disease_example <- dis %>%
  filter(disease == "Measles" & state == 'Florida')

disease_2 <- disease %>%
  filter(disease == "Measles")

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

fills = setNames(
  c(RColorBrewer::brewer.pal(5, 'YlOrRd'), 'white'),
  c(LETTERS[1:4], 'defaultFill')
)
disease_ex2 <- dlply(na.omit(disease_example), "year", function(x){
  y = toJSONArray2(x, json = F)
  names(y) = lapply(y, '[[', 'state')
  return(y)
})
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

source('ichoropleth.R')
ichoropleth(count ~ state,
            data = disease_example,
            pal = 'Blues',
            ncuts = 5,
            animate = 'year',
            play = F
)
