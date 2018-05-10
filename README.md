# Data Visualization Take-Home Final

Link to shiny app: [here](https://susmani.shinyapps.io/dataviz_final/)

I attempted to answer the following questions: 

1)How are overall disease trends changing over time? 

2)Are some diseases more of a threat today than others?

3)Is there a geographic pattern to illnesses?

I created three separate visualizations to try to answer that. The disease is the central component that is utilized across all three visualizations. On the first tab, you can choose the disease you want to focus on, and then examine it by state over time. The second tab will use the same disease to examine it over time across all states. The third tab is a geographic visualization that uses the chosen disease and examine where this disease has spread across over time in which areas and in which intensity. 

I spent about **12-15** hours on this, mainly because I first tried to create a choropleth map using a javascript-R plug in which worked fine on RStudio but was undeployable on the Shiny app. Thus, I had to completely switch to Leaflet to try to implement an animation geogrpahically through that way. I still included my first attempt on the "base code.R" file which you can run. It looks really nice and is faster than the leaflet I ended up using, but everything turned out nice in the end. 

I hope you enjoy!
